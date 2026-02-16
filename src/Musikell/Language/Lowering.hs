-- File: src/Musikell/Language/Lowering.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/language/lowering.mdx
-- Module: Musikell.Language.Lowering
{-# LANGUAGE OverloadedStrings #-}

-- | Lower a parsed AST into a runnable audio graph.
--
-- The lowering pass (v2):
--   1. Walks every statement in program order.
--   2. Handles imports, groups, parameter sets, bindings, I/O.
--   3. Evaluates arithmetic expressions (including min/max/clamp).
--   4. Resolves multi-input references and feedback (~) refs.
--   5. Builds a graph with both forward and feedback edges.
--   6. Creates auto-mix nodes for groups used as inputs.
--   7. Tracks bypass/mute prefixes for runtime toggling.
module Musikell.Language.Lowering (
    -- * Lowering
    lowerProgram,

    -- * Result
    LoweringResult (..),

    -- * Errors
    LoweringError (..),
) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import System.Directory (doesFileExist)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Musikell.Core.ExecutionPlan (ExecutionPlan, buildPlan)
import Musikell.Core.Graph (
    Graph,
    addEdge,
    addNode,
    emptyGraph,
    mkEdge,
    mkFeedbackEdge,
 )
import Musikell.Core.Node (NodeSpec, mkNodeSpec, mkPort)
import Musikell.Core.Types (
    BlockSize,
    KernelId (..),
    NodeId (..),
    PortId (..),
    SampleRate,
    defaultBlockSize,
    defaultSampleRate,
 )
import Musikell.Language.AST
import Musikell.Language.Parser (parseProgram)
import Musikell.Runtime.Kernel (
    KernelRegistry,
    KernelSpec (..),
    lookupKernel,
    registerKernel,
 )
import Musikell.Runtime.Kernel.Builtin (
    builtinRegistry,
    channelSplitKernel,
    clipKernel,
    gainKernel,
    inputKernel,
    mixKernel,
    mkBeatSequencer,
    mkCrossfade,
    mkDelay,
    mkOscillatorSawtooth,
    mkOscillatorSine,
    mkOscillatorSquare,
    outputKernel,
    passthruKernel,
 )

-- ═══════════════════════════════════════════════════════════════════
-- Types
-- ═══════════════════════════════════════════════════════════════════

data LoweringError
    = UndefinedVariable Text
    | TypeMismatch Text Text
    | InvalidNodeConfiguration Text
    | GraphConstructionFailed Text
    | ExprEvalError Text
    | ImportError Text
    deriving (Eq, Show)

data LoweringResult = LoweringResult
    { lowerGraph :: !Graph,
      lowerPlan :: !ExecutionPlan,
      lowerRegistry :: !KernelRegistry,
      -- | Node whose output buffer receives stdin
      lowerInputNode :: !NodeId,
      -- | Node whose input buffer becomes stdout
      lowerOutputNode :: !NodeId,
      -- | Nodes marked with ! bypass prefix
      lowerBypassSet :: !(Set NodeId),
      -- | Nodes marked with # mute prefix
      lowerMuteSet :: !(Set NodeId)
    }
    deriving (Show)

-- ═══════════════════════════════════════════════════════════════════
-- Environment entry
-- ═══════════════════════════════════════════════════════════════════

data EnvEntry = EnvEntry
    { envNodeId :: !NodeId,
      -- | Number of output channels
      envChannels :: !Int
    }
    deriving (Show)

-- ═══════════════════════════════════════════════════════════════════
-- Group info
-- ═══════════════════════════════════════════════════════════════════

data GroupInfo = GroupInfo
    { -- | All member node IDs
      groupMembers :: ![NodeId],
      -- | Auto-generated mixer (created on demand)
      groupMixNode :: !(Maybe NodeId)
    }
    deriving (Show)

-- ═══════════════════════════════════════════════════════════════════
-- Internal state
-- ═══════════════════════════════════════════════════════════════════

data LState = LState
    { lsNextId :: !Int,
      lsGraph :: !Graph,
      lsRegistry :: !KernelRegistry,
      lsEnv :: !(Map Text EnvEntry), -- variable → env entry
      lsInputs :: ![NodeId], -- input nodes (for stdin)
      lsOutputs :: ![NodeId], -- output nodes (for stdout)
      lsParamSets :: !(Map Text [ParamPair]), -- parameter set storage
      lsGroups :: !(Map Text GroupInfo), -- group name → info
      lsFeedbacks :: ![(NodeId, NodeId)], -- feedback edge pairs (src, dst)
      lsBypassSet :: !(Set NodeId), -- nodes with ! prefix
      lsMuteSet :: !(Set NodeId), -- nodes with # prefix
      lsPrefix :: !Text -- current scope prefix (e.g. "group.")
    }

freshId :: LState -> (NodeId, LState)
freshId st = (NodeId (lsNextId st), st {lsNextId = lsNextId st + 1})

addNodeL :: NodeSpec -> LState -> Either LoweringError LState
addNodeL spec st = case addNode spec (lsGraph st) of
    Left ge -> Left $ GraphConstructionFailed (T.pack (show ge))
    Right g' -> Right st {lsGraph = g'}

addEdgeL :: NodeId -> PortId -> NodeId -> PortId -> LState -> Either LoweringError LState
addEdgeL sn sp dn dp st = case addEdge (mkEdge sn sp dn dp) (lsGraph st) of
    Left ge -> Left $ GraphConstructionFailed (T.pack (show ge))
    Right g' -> Right st {lsGraph = g'}

addFeedbackEdgeL :: NodeId -> PortId -> NodeId -> PortId -> LState -> Either LoweringError LState
addFeedbackEdgeL sn sp dn dp st = case addEdge (mkFeedbackEdge sn sp dn dp) (lsGraph st) of
    Left ge -> Left $ GraphConstructionFailed (T.pack (show ge))
    Right g' -> Right st {lsGraph = g'}

-- | Bind a variable with full scope prefix
bindVar :: Text -> NodeId -> Int -> LState -> LState
bindVar name nid channels st =
    let fullName = if T.null (lsPrefix st) then name else lsPrefix st <> name
        entry = EnvEntry nid channels
    in  st {lsEnv = Map.insert fullName entry (lsEnv st)}

regKernel :: KernelSpec -> LState -> LState
regKernel ks st = st {lsRegistry = registerKernel ks (lsRegistry st)}

lookupVar :: Text -> LState -> Either LoweringError EnvEntry
lookupVar name st = case Map.lookup name (lsEnv st) of
    Nothing -> Left $ UndefinedVariable name
    Just ent -> Right ent

-- | Resolve a QualName to its full text representation
resolveQualName :: QualName -> Text
resolveQualName (QualName parts) = T.intercalate "." parts

-- ═══════════════════════════════════════════════════════════════════
-- Expression evaluation
-- ═══════════════════════════════════════════════════════════════════

-- | Evaluate a constant expression to a Double.
-- Used for parameter values, overrides, etc.
evalExpr :: LState -> Expr -> Either LoweringError Double
evalExpr st expr = case expr of
    ExprLit (LitNumber n) -> Right n
    ExprLit (LitBool True) -> Right 1.0
    ExprLit (LitBool False) -> Right 0.0
    ExprVar qn -> do
        let name = resolveQualName qn
        -- Try as param set field (e.g. VENUE.room_size)
        case qualParts qn of
            [setName, field] -> case Map.lookup setName (lsParamSets st) of
                Just pairs -> case findPair field pairs of
                    Just val -> evalExpr st (locValue val)
                    Nothing -> Left $ ExprEvalError ("unknown param '" <> field <> "' in set '" <> setName <> "'")
                Nothing -> Left $ ExprEvalError ("cannot evaluate variable '" <> name <> "' as number")
            _ -> Left $ ExprEvalError ("cannot evaluate variable '" <> name <> "' as number")
    ExprBinOp op lhs rhs -> do
        l <- evalExpr st (locValue lhs)
        r <- evalExpr st (locValue rhs)
        Right $ case op of
            OpAdd -> l + r
            OpSub -> l - r
            OpMul -> l * r
            OpDiv -> if r == 0 then 0 else l / r
    ExprUnaryNeg inner -> do
        v <- evalExpr st (locValue inner)
        Right (negate v)
    ExprParen inner -> evalExpr st (locValue inner)
    ExprCall "min" [a, b] -> do
        va <- evalExpr st (locValue a)
        vb <- evalExpr st (locValue b)
        Right (min va vb)
    ExprCall "max" [a, b] -> do
        va <- evalExpr st (locValue a)
        vb <- evalExpr st (locValue b)
        Right (max va vb)
    ExprCall "clamp" [val, lo, hi] -> do
        v <- evalExpr st (locValue val)
        vl <- evalExpr st (locValue lo)
        vh <- evalExpr st (locValue hi)
        Right (max vl (min vh v))
    _ -> Left $ ExprEvalError "expression cannot be evaluated to a constant"

findPair :: Text -> [ParamPair] -> Maybe (Located Expr)
findPair key = go
    where
        go [] = Nothing
        go (ParamPair k v : rest)
            | k == key = Just v
            | otherwise = go rest

-- ═══════════════════════════════════════════════════════════════════
-- Core lowering
-- ═══════════════════════════════════════════════════════════════════

lowerProgram :: KernelRegistry -> Program -> IO (Either LoweringError LoweringResult)
lowerProgram registry (Program imports stmts) = do
    let st0 =
            LState
                { lsNextId = 0,
                  lsGraph = emptyGraph,
                  lsRegistry = registry,
                  lsEnv = Map.empty,
                  lsInputs = [],
                  lsOutputs = [],
                  lsParamSets = Map.empty,
                  lsGroups = Map.empty,
                  lsFeedbacks = [],
                  lsBypassSet = Set.empty,
                  lsMuteSet = Set.empty,
                  lsPrefix = ""
                }
    -- 1. Process imports first
    importResult <- foldImports st0 imports
    case importResult of
        Left e -> pure (Left e)
        Right st1 -> do
            -- 2. Then process statements
            result <- foldStmts st1 stmts
            case result of
                Left e -> pure (Left e)
                Right st -> finalise st

foldStmts :: LState -> [Located Statement] -> IO (Either LoweringError LState)
foldStmts st [] = pure (Right st)
foldStmts st (Located _ s : rest) = do
    result <- lowerStmt st s
    case result of
        Left e -> pure (Left e)
        Right st' -> foldStmts st' rest

-- ───────────────────────────────────────────────────────────────────
-- Statement lowering
-- ───────────────────────────────────────────────────────────────────

lowerStmt :: LState -> Statement -> IO (Either LoweringError LState)
-- Comments are no-ops.
lowerStmt st (CommentStmt _) = pure (Right st)
-- Parameter set: store for later evaluation
lowerStmt st (ParamSet name pairs) = do
    pure $ Right st {lsParamSets = Map.insert name pairs (lsParamSets st)}

-- I/O statements
lowerStmt st (IOStmt StreamIn ident _mChannel) = do
    let (nid, st1) = freshId st
        spec =
            mkNodeSpec
                nid
                [] -- no input ports
                [mkPort (PortId 0) "out"] -- one output
                (KernelId "input")
    case addNodeL spec st1 of
        Left e -> pure (Left e)
        Right st2 ->
            pure
                $ Right
                $ (bindVar ident nid 1 st2) {lsInputs = nid : lsInputs st2}
lowerStmt st (IOStmt StreamOut ident _mChannel) = do
    let srcName = ident
    case lookupVar srcName st of
        Left e -> pure (Left e)
        Right srcEntry -> do
            let (nid, st1) = freshId st
                spec =
                    mkNodeSpec
                        nid
                        [mkPort (PortId 0) "in"]
                        [mkPort (PortId 0) "out"]
                        (KernelId "output")
            case addNodeL spec st1 >>= addEdgeL (envNodeId srcEntry) (PortId 0) nid (PortId 0) of
                Left e -> pure (Left e)
                Right st2 ->
                    pure
                        $ Right
                        $ st2 {lsOutputs = nid : lsOutputs st2}

-- Group definition
lowerStmt st (GroupDef name stmts) = do
    let savedPrefix = lsPrefix st
        newPrefix = if T.null savedPrefix then name <> "." else savedPrefix <> name <> "."
        st1 = st {lsPrefix = newPrefix}
    result <- foldStmts st1 stmts
    case result of
        Left e -> pure (Left e)
        Right st2 -> do
            -- Collect member nodes that were added in this group scope
            let memberNodes =
                    [ envNodeId entry
                    | (k, entry) <- Map.toList (lsEnv st2),
                      T.isPrefixOf newPrefix k
                    ]
                gInfo = GroupInfo memberNodes Nothing
                st3 =
                    st2
                        { lsPrefix = savedPrefix,
                          lsGroups =
                            Map.insert
                                (if T.null savedPrefix then name else savedPrefix <> name)
                                gInfo
                                (lsGroups st2)
                        }
            pure (Right st3)

-- Binding: create a node from the expression
lowerStmt st (Binding prefix ident expr) = lowerBinding st prefix ident expr

-- ───────────────────────────────────────────────────────────────────
-- Binding lowering
-- ───────────────────────────────────────────────────────────────────

lowerBinding :: LState -> BindingPrefix -> Identifier -> Expr -> IO (Either LoweringError LState)
-- Variable alias
lowerBinding st prefix ident (ExprVar qn) = do
    let refName = resolveQualName qn
    case lookupVar refName st of
        Left e -> pure (Left e)
        Right entry -> do
            let st1 = bindVar ident (envNodeId entry) (envChannels entry) st
            pure $ Right $ applyPrefix prefix (envNodeId entry) st1

-- Literal — numeric value creates a constant-output node
lowerBinding _ _ _ (ExprLit _) =
    pure $ Left $ InvalidNodeConfiguration "cannot assign a literal to a node binding"
-- Node expression — the real work
lowerBinding st prefix ident (ExprNode ne) = do
    let (nid, st1) = freshId st

    -- Resolve inputs (may create auto-mix nodes, updating state)
    inputResults <- resolveInputs st1 (neInputs ne)
    case inputResults of
        Left e -> pure (Left e)
        Right (resolvedInputs, st1') -> do
            -- Determine kernel from neKernel (evaluate args with state for param set refs)
            let kernelName = resolveQualName (neKernel ne)
                argMap = extractNamedArgsWithState st1' (neArgs ne)

            -- Evaluate overrides and merge into argMap
            let overrideArgs = evalOverrides st1' (neOverrides ne) argMap

            -- Create kernel
            eKernel <- createKernel (lsRegistry st1') kernelName overrideArgs st1' (neArgs ne)

            case eKernel of
                Left e -> pure (Left e)
                Right ks -> do
                    let kid = kernelId ks
                        inPorts =
                            [ mkPort (PortId i) ("in" <> T.pack (show i))
                            | i <- [0 .. length resolvedInputs - 1]
                            ]
                        outPorts = [mkPort (PortId 0) "out"]
                        spec = mkNodeSpec nid inPorts outPorts kid

                    let st2 = regKernel ks st1'

                    case addNodeL spec st2 of
                        Left e -> pure (Left e)
                        Right st3 -> do
                            -- Wire input edges
                            let wireInputs s [] _ = pure (Right s)
                                wireInputs s ((srcNid, isFeedback) : rest) idx =
                                    if isFeedback then case addFeedbackEdgeL srcNid (PortId 0) nid (PortId idx) s of
                                        Left e -> pure (Left e)
                                        Right s' -> wireInputs s' rest (idx + 1)
                                    else case addEdgeL srcNid (PortId 0) nid (PortId idx) s of
                                        Left e -> pure (Left e)
                                        Right s' -> wireInputs s' rest (idx + 1)

                            wireResult <- wireInputs st3 resolvedInputs 0
                            case wireResult of
                                Left e -> pure (Left e)
                                Right st4 -> do
                                    -- Apply method chains
                                    methodResult <- applyMethods st4 nid (neMethods ne)
                                    case methodResult of
                                        Left e -> pure (Left e)
                                        Right (finalNid, st5) -> do
                                            let st6 = bindVar ident finalNid 1 st5
                                                st7 = applyPrefix prefix finalNid st6
                                            pure (Right st7)

-- Dot access — channel extraction (.L, .R)
lowerBinding st prefix ident (ExprDotAccess inner field) = do
    case T.toLower field of
        "l" -> lowerChannelSplit st prefix ident (locValue inner) 0
        "r" -> lowerChannelSplit st prefix ident (locValue inner) 1
        _ ->
            pure
                $ Left
                $ InvalidNodeConfiguration
                    ("unsupported dot access '." <> field <> "' in binding for '" <> ident <> "'")

-- Any other expression form
lowerBinding _st _prefix ident _expr = do
    -- For expressions like function calls, bin ops, etc. — try to create
    -- a passthrough node with a computed value
    pure
        $ Left
        $ InvalidNodeConfiguration
            ("unsupported expression form in binding for '" <> ident <> "'")

-- ───────────────────────────────────────────────────────────────────
-- Input resolution
-- ───────────────────────────────────────────────────────────────────

-- | Resolve input references to (NodeId, isFeedback) pairs.
-- When a group name is referenced and has no mix node yet, create one.
resolveInputs :: LState -> [InputRef] -> IO (Either LoweringError ([(NodeId, Bool)], LState))
resolveInputs st refs = go st refs []
    where
        go s [] acc = pure (Right (reverse acc, s))
        go s (ref : rest) acc = case resolveOneInput s ref of
            Right (nid, isFb, s') -> go s' rest ((nid, isFb) : acc)
            Left e -> pure (Left e)

resolveOneInput :: LState -> InputRef -> Either LoweringError (NodeId, Bool, LState)
resolveOneInput st (NormalRef qn) = do
    let name = resolveQualName qn
    -- Check if it's a group name (auto-mix)
    case Map.lookup name (lsGroups st) of
        Just gInfo -> case groupMixNode gInfo of
            Just mixNid -> Right (mixNid, False, st)
            Nothing ->
                -- Auto-generate a mix node for the group
                case groupMembers gInfo of
                    [] -> Left $ UndefinedVariable name
                    members -> createGroupMix st name gInfo members
        Nothing -> do
            entry <- lookupVar name st
            Right (envNodeId entry, False, st)
resolveOneInput st (FeedbackRef qn) = do
    let name = resolveQualName qn
    case lookupVar name st of
        Right entry -> Right (envNodeId entry, True, st)
        Left _ -> Left $ UndefinedVariable ("feedback ref ~" <> name)

-- | Create an auto-mix node for a group with multiple members.
createGroupMix :: LState -> Text -> GroupInfo -> [NodeId] -> Either LoweringError (NodeId, Bool, LState)
createGroupMix st name gInfo members = do
    let (mixNid, st1) = freshId st
        ks = mixKernel
        kid = kernelId ks
        inPorts =
            [ mkPort (PortId i) ("in" <> T.pack (show i))
            | i <- [0 .. length members - 1]
            ]
        outPorts = [mkPort (PortId 0) "out"]
        spec = mkNodeSpec mixNid inPorts outPorts kid
        st2 = regKernel ks st1
    st3 <- addNodeL spec st2
    -- Wire each member to the mix node
    st4 <- wireMembers st3 members mixNid 0
    -- Update group info with the mix node
    let gInfo' = gInfo {groupMixNode = Just mixNid}
        st5 = st4 {lsGroups = Map.insert name gInfo' (lsGroups st4)}
    Right (mixNid, False, st5)
    where
        wireMembers s [] _ _ = Right s
        wireMembers s (m : ms) mixN idx =
            case addEdgeL m (PortId 0) mixN (PortId idx) s of
                Left e -> Left e
                Right s' -> wireMembers s' ms mixN (idx + 1)

-- ───────────────────────────────────────────────────────────────────
-- Prefix application
-- ───────────────────────────────────────────────────────────────────

applyPrefix :: BindingPrefix -> NodeId -> LState -> LState
applyPrefix PrefixNone _ st = st
applyPrefix PrefixBypass nid st = st {lsBypassSet = Set.insert nid (lsBypassSet st)}
applyPrefix PrefixMute nid st = st {lsMuteSet = Set.insert nid (lsMuteSet st)}

-- ───────────────────────────────────────────────────────────────────
-- Channel split lowering
-- ───────────────────────────────────────────────────────────────────

-- | Lower a dot-access channel extraction (.L or .R) into a channel split node.
lowerChannelSplit ::
    LState ->
    BindingPrefix ->
    Identifier ->
    Expr ->
    Int ->
    IO (Either LoweringError LState)
lowerChannelSplit st prefix ident innerExpr channel = do
    -- Resolve the source node from the inner expression
    case innerExpr of
        ExprVar qn -> do
            let srcName = resolveQualName qn
            case lookupVar srcName st of
                Left e -> pure (Left e)
                Right srcEntry -> do
                    let (nid, st1) = freshId st
                        ks = channelSplitKernel channel (envChannels srcEntry)
                        kid = kernelId ks
                        spec =
                            mkNodeSpec
                                nid
                                [mkPort (PortId 0) "in"]
                                [mkPort (PortId 0) "out"]
                                kid
                        st2 = regKernel ks st1
                    case addNodeL spec st2 >>= addEdgeL (envNodeId srcEntry) (PortId 0) nid (PortId 0) of
                        Left e -> pure (Left e)
                        Right st3 -> do
                            let st4 = bindVar ident nid 1 st3
                                st5 = applyPrefix prefix nid st4
                            pure (Right st5)
        _ ->
            pure
                $ Left
                $ InvalidNodeConfiguration
                    "channel split requires a variable reference, got complex expression"

-- ───────────────────────────────────────────────────────────────────
-- Method chain application
-- ───────────────────────────────────────────────────────────────────

-- | Apply method chain (.switch, .case, .mute) creating nodes wired in series.
-- Returns the final node ID and updated state.
applyMethods :: LState -> NodeId -> [MethodCall] -> IO (Either LoweringError (NodeId, LState))
applyMethods st currentNid [] = pure (Right (currentNid, st))
applyMethods st currentNid (mc : rest) = do
    result <- applyOneMethod st currentNid mc
    case result of
        Left e -> pure (Left e)
        Right (newNid, st') -> applyMethods st' newNid rest

applyOneMethod :: LState -> NodeId -> MethodCall -> IO (Either LoweringError (NodeId, LState))
applyOneMethod st srcNid (MethodCall "case" _mTp _args) = do
    -- .case() creates a conditional branch — for now, wire as passthru
    -- (full conditional routing is in Phase 7)
    let (nid, st1) = freshId st
        ks = passthruKernel {kernelId = KernelId "case"}
        spec =
            mkNodeSpec
                nid
                [mkPort (PortId 0) "in"]
                [mkPort (PortId 0) "out"]
                (kernelId ks)
        st2 = regKernel ks st1
    case addNodeL spec st2 >>= addEdgeL srcNid (PortId 0) nid (PortId 0) of
        Left e -> pure (Left e)
        Right st3 -> pure (Right (nid, st3))
applyOneMethod st srcNid (MethodCall "mute" _mTp _args) = do
    -- .mute() creates a mute gate node
    let (nid, st1) = freshId st
        ks = gainKernel 0.0
        kid = KernelId "mute"
        ks' = ks {kernelId = kid}
        spec =
            mkNodeSpec
                nid
                [mkPort (PortId 0) "in"]
                [mkPort (PortId 0) "out"]
                kid
        st2 = regKernel ks' st1
    case addNodeL spec st2 >>= addEdgeL srcNid (PortId 0) nid (PortId 0) of
        Left e -> pure (Left e)
        Right st3 -> pure (Right (nid, st3))
applyOneMethod st srcNid (MethodCall _name _mTp _args) = do
    -- Generic method: create a passthru placeholder wired in series
    let (nid, st1) = freshId st
        ks = passthruKernel
        spec =
            mkNodeSpec
                nid
                [mkPort (PortId 0) "in"]
                [mkPort (PortId 0) "out"]
                (kernelId ks)
        st2 = regKernel ks st1
    case addNodeL spec st2 >>= addEdgeL srcNid (PortId 0) nid (PortId 0) of
        Left e -> pure (Left e)
        Right st3 -> pure (Right (nid, st3))

-- ───────────────────────────────────────────────────────────────────
-- Import processing
-- ───────────────────────────────────────────────────────────────────

-- | Process import declarations: parse referenced files and merge bindings.
foldImports :: LState -> [Located Import] -> IO (Either LoweringError LState)
foldImports st [] = pure (Right st)
foldImports st (Located _ imp : rest) = do
    result <- lowerImport st imp
    case result of
        Left e -> pure (Left e)
        Right st' -> foldImports st' rest

lowerImport :: LState -> Import -> IO (Either LoweringError LState)
lowerImport st (ImportAll path) = do
    eProgram <- parseImportFile path
    case eProgram of
        Left e -> pure (Left e)
        Right prog -> do
            eResult <- lowerProgram (lsRegistry st) prog
            case eResult of
                Left e -> pure (Left e)
                Right lr -> pure $ Right $ mergeImportAll st lr
lowerImport st (ImportNames path names) = do
    eProgram <- parseImportFile path
    case eProgram of
        Left e -> pure (Left e)
        Right prog -> do
            eResult <- lowerProgram (lsRegistry st) prog
            case eResult of
                Left e -> pure (Left e)
                Right lr -> pure $ Right $ mergeImportNames st lr names
lowerImport st (ImportAs path ns) = do
    eProgram <- parseImportFile path
    case eProgram of
        Left e -> pure (Left e)
        Right prog -> do
            eResult <- lowerProgram (lsRegistry st) prog
            case eResult of
                Left e -> pure (Left e)
                Right lr -> pure $ Right $ mergeImportAs st lr ns

-- | Try to parse an imported .mkl file. Returns a stub on error
-- (since imported files may not exist during lowering tests).
parseImportFile :: FilePath -> IO (Either LoweringError Program)
parseImportFile path = do
    eResult <- parseFileSafe path
    case eResult of
        Left msg -> pure $ Left $ ImportError (T.pack msg)
        Right prog -> pure $ Right prog

parseFileSafe :: FilePath -> IO (Either String Program)
parseFileSafe path = do
    exists <- doesFileExist path
    if not exists then
        pure $ Left $ "import file not found: " ++ path
    else do
        source <- TIO.readFile path
        case parseProgram path source of
            Left pe -> pure $ Left $ show pe
            Right prog -> pure $ Right prog

-- | Merge all bindings from an imported result into the current state.
mergeImportAll :: LState -> LoweringResult -> LState
mergeImportAll st _lr = st -- TODO: merge graph nodes and env bindings

-- | Merge selected names from an imported result.
mergeImportNames :: LState -> LoweringResult -> [Identifier] -> LState
mergeImportNames st _lr _names = st -- TODO: merge selected bindings

-- | Merge bindings with a namespace prefix.
mergeImportAs :: LState -> LoweringResult -> Identifier -> LState
mergeImportAs st _lr _ns = st -- TODO: merge with namespace prefix

-- ───────────────────────────────────────────────────────────────────
-- Kernel construction
-- ───────────────────────────────────────────────────────────────────

-- | Extract named args, evaluating expressions against current state.
-- Falls back to literal extraction for non-evaluable args.
extractNamedArgsWithState :: LState -> [NamedArg] -> Map Text Double
extractNamedArgsWithState st = foldl go Map.empty
    where
        go m (NamedArg k v) = case evalExpr st (locValue v) of
            Right n -> Map.insert k n m
            Left _ -> m -- non-evaluable args skipped

extractNamedArgs :: [NamedArg] -> Map Text Double
extractNamedArgs = foldl go Map.empty
    where
        go m (NamedArg k v) = case locValue v of
            ExprLit (LitNumber n) -> Map.insert k n m
            _ -> m

-- | Evaluate override expressions and merge into arg map.
-- Overrides of the form (PARAM_NAME=expr) or (PARAM_NAME * expr) are
-- evaluated and inserted. Positional overrides are mapped by index to
-- the "AMPLITUDE" or positional parameter keys.
evalOverrides :: LState -> [Located Expr] -> Map Text Double -> Map Text Double
evalOverrides st overrides argMap = foldl applyOverride argMap (zip [0 ..] overrides)
    where
        applyOverride m (idx, locExpr) =
            case locValue locExpr of
                -- Named override: AMPLITUDE=0.8 or AMPLITUDE * 0.9
                ExprBinOp OpMul (Located _ (ExprVar qn)) rhs ->
                    let paramName = T.toLower (resolveQualName qn)
                    in  case evalExpr st (ExprBinOp OpMul (Located (locPos locExpr) (ExprVar qn)) rhs) of
                            Right v -> Map.insert paramName v m
                            Left _ -> m
                -- Direct assignment-like override
                ExprVar qn ->
                    case qualParts qn of
                        [paramName] -> case evalExpr st (locValue locExpr) of
                            Right v -> Map.insert (T.toLower paramName) v m
                            Left _ -> m
                        _ -> m
                -- Numeric override by position
                _ -> case evalExpr st (locValue locExpr) of
                    Right v ->
                        let key = if idx == 0 then "amplitude" else "override_" <> T.pack (show (idx :: Int))
                        in  Map.insert key v m
                    Left _ -> m

getDouble :: Text -> Double -> Map Text Double -> Double
getDouble key def = Map.findWithDefault def key

createKernel ::
    KernelRegistry ->
    Text ->
    Map Text Double ->
    LState ->
    [NamedArg] ->
    IO (Either LoweringError KernelSpec)
createKernel registry name args _st rawArgs =
    case lookupKernel (KernelId name) registry of
        Just ks -> pure (Right ks)
        Nothing -> createBuiltinKernel name args rawArgs

createBuiltinKernel :: Text -> Map Text Double -> [NamedArg] -> IO (Either LoweringError KernelSpec)
createBuiltinKernel name args rawArgs = case T.toLower name of
    "oscillator" -> do
        let freq = getDouble "freq" 440.0 args
            sr = round $ getDouble "sample_rate" (fromIntegral defaultSampleRate) args
            waveform = extractLitIdent "waveform" rawArgs
        case waveform of
            Just "square" -> Right <$> mkOscillatorSquare freq sr
            Just "sawtooth" -> Right <$> mkOscillatorSawtooth freq sr
            Just "saw" -> Right <$> mkOscillatorSawtooth freq sr
            Just "triangle" -> Right <$> mkOscillatorSawtooth freq sr -- approximate with saw for now
            _ -> Right <$> mkOscillatorSine freq sr
    "gain" -> do
        let level = getDouble "level" 1.0 args
        pure $ Right $ gainKernel level
    "mixer" -> pure $ Right mixKernel
    "clip" -> do
        let lo = getDouble "lo" (-1.0) args
            hi = getDouble "hi" 1.0 args
        pure $ Right $ clipKernel lo hi
    "delay" -> do
        let samples = round $ getDouble "samples" 4410.0 args
        Right <$> mkDelay samples defaultBlockSize
    "passthru" -> pure $ Right passthruKernel
    "beat" -> do
        let bpm = getDouble "bpm" 128.0 args
            division = round $ getDouble "division" 16.0 args
            patStr = extractLitPattern rawArgs
        Right <$> mkBeatSequencer (T.unpack patStr) bpm division defaultBlockSize defaultSampleRate

    -- For unknown kernels (plugins, external), create a passthru placeholder
    -- The actual kernel would be resolved from the registry or loaded at runtime
    other -> do
        let kid = KernelId other
        pure
            $ Right
            $ KernelSpec
                { kernelId = kid,
                  kernelInputs = 1,
                  kernelOutputs = 1,
                  kernelExecute = \_ _ -> pure () -- no-op placeholder
                }

-- | Extract a literal identifier value from named args (e.g. waveform=sine)
extractLitIdent :: Text -> [NamedArg] -> Maybe Text
extractLitIdent key = go
    where
        go [] = Nothing
        go (NamedArg k v : rest)
            | k == key = case locValue v of
                ExprLit (LitIdent val) -> Just val
                ExprVar (QualName [val]) -> Just val
                _ -> go rest
            | otherwise = go rest

-- | Extract a beat pattern from named args
extractLitPattern :: [NamedArg] -> Text
extractLitPattern = go
    where
        go [] = "x___"
        go (NamedArg k v : rest)
            | k == "pattern" = case locValue v of
                ExprLit (LitPattern pat) -> pat
                _ -> go rest
            | otherwise = go rest

-- ═══════════════════════════════════════════════════════════════════
-- Finalisation
-- ═══════════════════════════════════════════════════════════════════

finalise :: LState -> IO (Either LoweringError LoweringResult)
finalise st = do
    case (lsInputs st, lsOutputs st) of
        ([], _) -> pure $ Left $ InvalidNodeConfiguration "no input nodes (need at least one <<<)"
        (_, []) -> pure $ Left $ InvalidNodeConfiguration "no output nodes (need at least one >>>)"
        (inNodes, outNodes) ->
            case buildPlan (lsGraph st) of
                Left pe -> pure $ Left $ GraphConstructionFailed (T.pack (show pe))
                Right plan ->
                    pure
                        $ Right
                        $ LoweringResult
                            { lowerGraph = lsGraph st,
                              lowerPlan = plan,
                              lowerRegistry = lsRegistry st,
                              lowerInputNode = last inNodes,
                              lowerOutputNode = head outNodes,
                              lowerBypassSet = lsBypassSet st,
                              lowerMuteSet = lsMuteSet st
                            }
