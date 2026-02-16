-- File: src/Musikell/Language/Parser.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/reference/language/parser.mdx
-- Module: Musikell.Language.Parser
{-# LANGUAGE OverloadedStrings #-}

-- | Hand-rolled recursive-descent parser for the Musikell DSL (.mkl).
--
-- The grammar (see Language.Grammar) supports the v2 syntax:
-- imports, groups, multi-input nodes, feedback refs, arithmetic
-- expressions, parameter sets, bypass/mute prefixes, beat patterns,
-- conditional sugar, and namespaced plugin kernels.
--
-- No external dependencies (no megaparsec, no attoparsec).
module Musikell.Language.Parser (
    -- * Entry points
    parseProgram,
    parseFile,

    -- * Error type
    ParseError (..),
) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isUpper)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Musikell.Language.AST

-- ═══════════════════════════════════════════════════════════════════
-- Error
-- ═══════════════════════════════════════════════════════════════════

data ParseError = ParseError
    { parseErrorPos :: !SourcePos,
      parseErrorMsg :: !Text
    }
    deriving (Eq, Show)

-- ═══════════════════════════════════════════════════════════════════
-- Parser monad  (State + Either, no mtl)
-- ═══════════════════════════════════════════════════════════════════

data PState = PState
    { psInput :: !Text,
      psPos :: !SourcePos
    }

newtype P a = P {runP :: PState -> Either ParseError (a, PState)}

instance Functor P where
    fmap f (P g) = P $ \s -> case g s of
        Left e -> Left e
        Right (a, s') -> Right (f a, s')

instance Applicative P where
    pure a = P $ \s -> Right (a, s)
    P f <*> P x = P $ \s -> case f s of
        Left e -> Left e
        Right (g, s') -> case x s' of
            Left e -> Left e
            Right (a, s'') -> Right (g a, s'')

instance Monad P where
    P m >>= k = P $ \s -> case m s of
        Left e -> Left e
        Right (a, s') -> runP (k a) s'

-- ───────────────────────────────────────────────────────────────────
-- Primitives
-- ───────────────────────────────────────────────────────────────────

err :: Text -> P a
err msg = P $ \s -> Left (ParseError (psPos s) msg)

getPos :: P SourcePos
getPos = P $ \s -> Right (psPos s, s)

peek :: P (Maybe Char)
peek = P $ \s -> case T.uncons (psInput s) of
    Nothing -> Right (Nothing, s)
    Just (c, _) -> Right (Just c, s)

-- | Peek at the next two characters
peek2 :: P (Maybe Char, Maybe Char)
peek2 = P $ \s -> case T.uncons (psInput s) of
    Nothing -> Right ((Nothing, Nothing), s)
    Just (c1, rest) -> case T.uncons rest of
        Nothing -> Right ((Just c1, Nothing), s)
        Just (c2, _) -> Right ((Just c1, Just c2), s)

advance :: P Char
advance = P $ \s -> case T.uncons (psInput s) of
    Nothing -> Left (ParseError (psPos s) "unexpected end of input")
    Just (c, rest) ->
        let pos = psPos s
            pos' =
                if c == '\n' then
                    pos {posLine = posLine pos + 1, posColumn = 1}
                else
                    pos {posColumn = posColumn pos + 1}
        in  Right (c, s {psInput = rest, psPos = pos'})

eof :: P ()
eof = P $ \s ->
    if T.null (psInput s) then
        Right ((), s)
    else
        Left (ParseError (psPos s) ("expected end of input, got: " <> T.take 20 (psInput s)))

satisfy :: (Char -> Bool) -> P Char
satisfy p = do
    mc <- peek
    case mc of
        Just c | p c -> advance
        Just c -> err ("unexpected character: " <> T.singleton c)
        Nothing -> err "unexpected end of input"

char :: Char -> P ()
char c = do
    _ <- satisfy (== c)
    pure ()

string :: Text -> P ()
string t = mapM_ char (T.unpack t)

optional :: P a -> P (Maybe a)
optional (P p) = P $ \s -> case p s of
    Left _ -> Right (Nothing, s)
    Right (a, s') -> Right (Just a, s')

try :: P a -> P a
try (P p) = P $ \s -> case p s of
    Left e -> Left (ParseError (psPos s) (parseErrorMsg e))
    Right r -> Right r

orElse :: P a -> P a -> P a
orElse (P a) (P b) = P $ \s -> case a s of
    Right r -> Right r
    Left _ -> b s

many :: P a -> P [a]
many p = P $ \s -> go [] s
    where
        go acc st = case runP p st of
            Left _ -> Right (reverse acc, st)
            Right (a, st') -> go (a : acc) st'

many1 :: P a -> P [a]
many1 p = do
    x <- p
    xs <- many p
    pure (x : xs)

-- ───────────────────────────────────────────────────────────────────
-- Whitespace
-- ───────────────────────────────────────────────────────────────────

-- | Skip spaces and tabs (NOT newlines).
skipHSpace :: P ()
skipHSpace = P $ \s -> Right ((), consumeHSpace s)
    where
        consumeHSpace st = case T.uncons (psInput st) of
            Just (c, rest)
                | c == ' ' || c == '\t' ->
                    consumeHSpace st {psInput = rest, psPos = (psPos st) {posColumn = posColumn (psPos st) + 1}}
            _ -> st

-- | Skip all whitespace including newlines.
skipWS :: P ()
skipWS = P $ \s -> Right ((), consumeWS s)
    where
        consumeWS st = case T.uncons (psInput st) of
            Just ('\n', rest) ->
                let pos = psPos st
                in  consumeWS st {psInput = rest, psPos = pos {posLine = posLine pos + 1, posColumn = 1}}
            Just (c, rest)
                | c == ' ' || c == '\t' || c == '\r' ->
                    consumeWS st {psInput = rest, psPos = (psPos st) {posColumn = posColumn (psPos st) + 1}}
            _ -> st

-- | Skip the rest of the current line (including the newline).
skipLine :: P ()
skipLine = P $ \s -> Right ((), dropLine s)
    where
        dropLine st = case T.uncons (psInput st) of
            Nothing -> st
            Just ('\n', rest) ->
                let pos = psPos st
                in  st {psInput = rest, psPos = pos {posLine = posLine pos + 1, posColumn = 1}}
            Just (_, rest) ->
                dropLine st {psInput = rest, psPos = (psPos st) {posColumn = posColumn (psPos st) + 1}}

skipOptionalNewline :: P ()
skipOptionalNewline = do
    mc <- peek
    case mc of
        Just '\n' -> do _ <- advance; pure ()
        Just '\r' -> do _ <- advance; skipOptionalNewline
        _ -> pure ()

-- ───────────────────────────────────────────────────────────────────
-- Tokens
-- ───────────────────────────────────────────────────────────────────

pIdentifier :: P Text
pIdentifier = do
    c <- satisfy (\c -> isAlpha c || c == '_')
    cs <- many (satisfy (\c -> isAlphaNum c || c == '_'))
    pure $ T.pack (c : cs)

pNumber :: P Double
pNumber = do
    intPart <- many1 (satisfy isDigit)
    mc <- peek
    case mc of
        Just '.' -> do
            _ <- advance
            fracPart <- many1 (satisfy isDigit)
            pure $ read (intPart ++ "." ++ fracPart)
        _ -> pure $ read intPart

pString :: P Text
pString = do
    char '"'
    cs <- many (satisfy (/= '"'))
    char '"'
    pure $ T.pack cs

pRestOfLine :: P Text
pRestOfLine = P $ \s ->
    let (line, rest) = T.break (== '\n') (psInput s)
        pos = psPos s
        newPos = case T.uncons rest of
            Just ('\n', _) -> pos {posLine = posLine pos + 1, posColumn = 1}
            _ -> pos {posColumn = posColumn pos + T.length line}
        rest' = case T.uncons rest of
            Just ('\n', r) -> r
            _ -> rest
    in  Right (line, s {psInput = rest', psPos = newPos})

-- ───────────────────────────────────────────────────────────────────
-- Qualified names and booleans
-- ───────────────────────────────────────────────────────────────────

-- | Parse a dot-qualified name: @ident(.ident)*@
pQualName :: P QualName
pQualName = do
    first <- pIdentifier
    rest <- many (try pDotIdent)
    pure $ QualName (first : rest)
    where
        -- Only parse .ident when the dot is followed by an alpha/underscore
        -- (to avoid consuming .switch, .case etc. when we don't want to)
        pDotIdent = do
            char '.'
            pIdentifier

-- | Parse a boolean keyword
pBool :: P Bool
pBool = do
    ident <- pIdentifier
    case ident of
        "true" -> pure True
        "false" -> pure False
        _ -> err ("expected 'true' or 'false', got '" <> ident <> "'")

-- | Parse a beat pattern: [xXoO_]+
pPattern :: P Text
pPattern = do
    cs <- many1 (satisfy isPatternChar)
    pure $ T.pack cs
    where
        isPatternChar c = c == 'x' || c == 'X' || c == 'o' || c == 'O' || c == '_'

-- ═══════════════════════════════════════════════════════════════════
-- Expressions
-- ═══════════════════════════════════════════════════════════════════

-- | Parse an expression using precedence climbing.
pExpr :: P (Located Expr)
pExpr = pExprPrec 0

-- | Precedence climbing parser.
-- Prec 0: + -
-- Prec 1: * /
pExprPrec :: Int -> P (Located Expr)
pExprPrec minPrec = do
    lhs <- pAtomExpr
    pBinOpLoop minPrec lhs

pBinOpLoop :: Int -> Located Expr -> P (Located Expr)
pBinOpLoop minPrec lhs = do
    skipHSpace
    mc <- peek
    case mc of
        Just '+' | minPrec <= 0 -> doBinOp OpAdd 0 lhs
        Just '-' -> do
            -- Disambiguate: '-' as subtraction vs '--' comment
            (_, mc2) <- peek2
            case mc2 of
                Just '-' -> pure lhs -- it's a comment, stop
                _ | minPrec <= 0 -> doBinOp OpSub 0 lhs
                _ -> pure lhs
        Just '*' | minPrec <= 1 -> doBinOp OpMul 1 lhs
        Just '/' | minPrec <= 1 -> doBinOp OpDiv 1 lhs
        _ -> pure lhs
    where
        doBinOp op prec lhsExpr = do
            pos <- getPos
            _ <- advance -- consume the operator
            skipHSpace
            rhs <- pExprPrec (prec + 1)
            let combined = Located pos (ExprBinOp op lhsExpr rhs)
            pBinOpLoop minPrec combined

-- | Parse an atomic expression (highest precedence).
pAtomExpr :: P (Located Expr)
pAtomExpr = do
    skipHSpace
    pos <- getPos
    mc <- peek
    atom <- case mc of
        Just '"' ->
            Located pos . ExprLit . LitString <$> pString
        Just '~' -> do
            _ <- advance
            Located pos . ExprFeedback <$> pQualName
        Just '(' -> do
            _ <- advance
            skipWS
            inner <- pExpr
            skipWS
            char ')'
            pure $ Located pos (ExprParen inner)
        Just '-' -> do
            -- Unary negation (not comment since that's handled at statement level)
            _ <- advance
            skipHSpace
            Located pos . ExprUnaryNeg <$> pAtomExpr
        Just c
            | isDigit c ->
                Located pos . ExprLit . LitNumber <$> pNumber
        Just c | isAlpha c || c == '_' -> do
            -- Could be: bool, node type keyword, function call, or variable ref
            pIdentStartExpr pos
        _ -> err "expected expression"
    -- Post-atom: dot access chaining (but not .Kernel — that's handled inside pNodeExpr)
    pDotAccessLoop atom

-- | Parse an expression starting with an identifier.
-- Disambiguates between node types, booleans, function calls, and variable refs.
pIdentStartExpr :: SourcePos -> P (Located Expr)
pIdentStartExpr pos = do
    ident <- pIdentifier
    case ident of
        "true" -> pure $ Located pos (ExprLit (LitBool True))
        "false" -> pure $ Located pos (ExprLit (LitBool False))
        "Creational" -> pNodeExprBody pos Creational
        "Manipulative" -> pNodeExprBody pos Manipulative
        "Conditional" -> pNodeExprBody pos Conditional
        -- Check for function call: ident(...)
        _ -> do
            skipHSpace
            mc <- peek
            case mc of
                Just '('
                    | isFunctionName ident ->
                        Located pos . ExprCall ident <$> pFuncArgList
                _ -> do
                    -- Could be qualified name: ident.ident.ident
                    rest <- many (try pDotIdentPart)
                    let qn = QualName (ident : rest)
                    pure $ Located pos (ExprVar qn)

-- | Built-in function names that are parsed as ExprCall
isFunctionName :: Text -> Bool
isFunctionName "min" = True
isFunctionName "max" = True
isFunctionName "clamp" = True
isFunctionName _ = False

-- | Parse .ident (for qualified name continuation)
pDotIdentPart :: P Identifier
pDotIdentPart = do
    char '.'
    mc <- peek
    case mc of
        Just c | isAlpha c || c == '_' -> pIdentifier
        _ -> err "expected identifier after '.'"

-- | Parse dot-access chain: .L, .R, .meta.field
pDotAccessLoop :: Located Expr -> P (Located Expr)
pDotAccessLoop expr = do
    mc <- peek
    case mc of
        Just '.' -> do
            -- Peek ahead to see if this could be a dot access on a non-node expression
            (_, mc2) <- peek2
            case mc2 of
                Just c
                    | isAlpha c && not (isUpper c) ->
                        -- lowercase after dot = field access (.L, .R, .meta, etc.)
                        -- But be careful: we can't just blindly parse this for node expressions
                        -- because .Kernel is parsed inside pNodeExprBody
                        ( do
                            _ <- advance
                            field <- pIdentifier
                            let pos = locPos expr
                            pDotAccessLoop (Located pos (ExprDotAccess expr field))
                        )
                            `orElse` pure expr
                _ -> pure expr
        _ -> pure expr

-- | Parse function call argument list: @(expr, expr, ...)@
pFuncArgList :: P [Located Expr]
pFuncArgList = do
    char '('
    skipWS
    mc <- peek
    case mc of
        Just ')' -> do char ')'; pure []
        _ -> do
            first <- pExpr
            rest <- many (do skipWS; char ','; skipWS; pExpr)
            skipWS
            char ')'
            pure (first : rest)

-- ═══════════════════════════════════════════════════════════════════
-- Node expressions
-- ═══════════════════════════════════════════════════════════════════

-- | Parse the body of a node expression after the NodeType keyword.
-- NodeType<inputs>(overrides).Kernel(args).method()...
pNodeExprBody :: SourcePos -> NodeType -> P (Located Expr)
pNodeExprBody pos nt = do
    skipHSpace
    -- Optional input list: <ref, ref, ...>
    inputs <- fromMaybe [] <$> optional pInputList
    skipHSpace
    -- Optional overrides: (expr, expr, ...)
    overrides <- fromMaybe [] <$> optional pOverrides
    skipHSpace
    -- Required: .Kernel (dot + uppercase-starting name, possibly qualified)
    char '.'
    kernel <- pKernelName
    -- Optional type parameter: <IDENT> (e.g. .switch<AMPLITUDE>)
    mKernelTp <- optional $ do
        char '<'
        tp <- pIdentifier
        char '>'
        pure tp
    skipHSpace
    -- Required: (named args)
    args <- pNamedArgList
    skipHSpace
    -- Optional: method chain .method<T>(args)...
    methods <- many (try pMethodCall)
    pure
        $ Located pos
        $ ExprNode
        $ NodeExpr
            { neType = nt,
              neInputs = inputs,
              neOverrides = overrides,
              neKernel = kernel,
              neKernelTypeParam = mKernelTp,
              neArgs = args,
              neMethods = methods
            }

-- | Parse kernel name: @Oscillator@ or @PremVerb.Hall@ (dot-qualified)
pKernelName :: P QualName
pKernelName = do
    first <- pIdentifier
    rest <- many (try pDotIdentPart)
    pure $ QualName (first : rest)

-- | Parse input list: @<ref, ref, ...>@
pInputList :: P [InputRef]
pInputList = do
    char '<'
    skipWS
    first <- pInputRef
    rest <- many (do skipWS; char ','; skipWS; pInputRef)
    skipWS
    char '>'
    pure (first : rest)

-- | Parse a single input reference (normal or feedback ~)
pInputRef :: P InputRef
pInputRef = do
    mc <- peek
    case mc of
        Just '~' -> do
            _ <- advance
            FeedbackRef <$> pQualName
        _ ->
            NormalRef <$> pQualName

-- | Parse override expressions: @(expr, expr, ...)@
-- These are the parens between <inputs> and .Kernel
pOverrides :: P [Located Expr]
pOverrides = do
    char '('
    skipWS
    mc <- peek
    case mc of
        Just ')' -> do char ')'; pure []
        _ -> do
            first <- pExpr
            rest <- many (do skipWS; char ','; skipWS; pExpr)
            skipWS
            char ')'
            pure (first : rest)

-- | Parse named argument list: @(key=expr, key=expr)@
pNamedArgList :: P [NamedArg]
pNamedArgList = do
    char '('
    skipWS
    mc <- peek
    case mc of
        Just ')' -> do char ')'; pure []
        _ -> do
            first <- pNamedArg
            rest <- many (do skipWS; char ','; skipWS; pNamedArg)
            skipWS
            char ')'
            pure (first : rest)

-- | Parse a single named argument: @key=expr@
-- Special case: @pattern=x_o_x_o_@ triggers beat pattern parsing
pNamedArg :: P NamedArg
pNamedArg = do
    pos <- getPos
    name <- pIdentifier
    skipHSpace
    char '='
    skipHSpace
    val <-
        if name == "pattern" then
            Located pos . ExprLit . LitPattern <$> pPattern
        else
            pExpr
    pure $ NamedArg name val

-- | Parse a method call: @.method[<TypeParam>](args)@
pMethodCall :: P MethodCall
pMethodCall = do
    skipHSpace
    -- Skip newlines + indentation before chained methods
    skipWS
    char '.'
    name <- pIdentifier
    -- Optional type parameter: <IDENT>
    mTypeParam <- optional $ do
        char '<'
        tp <- pIdentifier
        char '>'
        pure tp
    -- Required: argument list
    MethodCall name mTypeParam <$> pNamedArgList

-- ═══════════════════════════════════════════════════════════════════
-- Statements
-- ═══════════════════════════════════════════════════════════════════

-- | Parse a sequence of statements (stops at EOF or '}')
pStatements :: P [Located Statement]
pStatements = many pStatementLine

-- | Try to parse one statement. Skips blank lines and whitespace.
pStatementLine :: P (Located Statement)
pStatementLine = do
    skipWS
    mc <- peek
    case mc of
        Nothing -> P $ \s -> Left (ParseError (psPos s) "no more statements")
        Just '}' -> P $ \s -> Left (ParseError (psPos s) "end of group")
        Just '-' -> do
            (_, mc2) <- peek2
            case mc2 of
                Just '-' -> pCommentLine
                _ -> pNonCommentStatement
        Just '!' -> pPrefixedBinding PrefixBypass
        Just '#' -> pPrefixedBinding PrefixMute
        _ -> pNonCommentStatement

-- | Parse a comment line: @-- text@
pCommentLine :: P (Located Statement)
pCommentLine = do
    pos <- getPos
    char '-'
    char '-'
    Located pos . CommentStmt <$> pRestOfLine

-- | Parse a binding with ! or # prefix
pPrefixedBinding :: BindingPrefix -> P (Located Statement)
pPrefixedBinding prefix = do
    pos <- getPos
    _ <- advance -- consume ! or #
    skipHSpace
    ident <- pIdentifier
    skipHSpace
    char '='
    skipHSpace
    expr <- pExpr
    skipHSpace
    skipOptionalNewline
    pure $ Located pos (Binding prefix ident (locValue expr))

-- | Parse a non-comment statement (dispatches on first token)
pNonCommentStatement :: P (Located Statement)
pNonCommentStatement = do
    pos <- getPos
    -- Check for 'use' (import) or 'group' keywords
    mc <- peek
    case mc of
        Just c | isAlpha c || c == '_' -> do
            ident <- pIdentifier
            case ident of
                "use" -> pImport pos
                "group" -> pGroupDef pos
                _ -> pAfterIdent pos ident
        _ -> err "expected statement"

-- | Parse an import statement after 'use'
pImport :: SourcePos -> P (Located Statement)
pImport _pos = do
    -- Imports are collected at program level, not as statements
    -- But we still need to parse them. They get handled specially in pProgram.
    err "imports should be parsed at program level"

-- | Parse import at program level
pImportDecl :: P (Located Import)
pImportDecl = do
    pos <- getPos
    string "use"
    skipHSpace
    path <- T.unpack <$> pString
    skipHSpace
    mc <- peek
    case mc of
        Just '(' -> do
            -- Selective import: use "file" (X, Y)
            char '('
            skipWS
            first <- pIdentifier
            rest <- many (do skipWS; char ','; skipWS; pIdentifier)
            skipWS
            char ')'
            skipHSpace
            skipOptionalNewline
            pure $ Located pos (ImportNames path (first : rest))
        Just 'a' -> do
            -- Namespaced: use "file" as NS
            string "as"
            skipHSpace
            ns <- pIdentifier
            skipHSpace
            skipOptionalNewline
            pure $ Located pos (ImportAs path ns)
        _ -> do
            -- Bare import: use "file"
            skipHSpace
            skipOptionalNewline
            pure $ Located pos (ImportAll path)

-- | Parse a group definition after 'group'
pGroupDef :: SourcePos -> P (Located Statement)
pGroupDef pos = do
    skipHSpace
    name <- pIdentifier
    skipHSpace
    skipWS
    char '{'
    stmts <- pStatements
    skipWS
    char '}'
    skipHSpace
    skipOptionalNewline
    pure $ Located pos (GroupDef name stmts)

-- | After parsing an identifier, figure out what kind of statement it is
pAfterIdent :: SourcePos -> Identifier -> P (Located Statement)
pAfterIdent pos ident = do
    skipHSpace
    mc <- peek
    case mc of
        Just '=' -> do
            -- Check for => (param set) vs = (binding)
            (_, mc2) <- peek2
            case mc2 of
                Just '>' -> pParamSetBody pos ident
                _ -> pBindingBody pos PrefixNone ident
        Just '<' -> do
            -- Check for <<< (input)
            (_, mc2) <- peek2
            case mc2 of
                Just '<' -> pIOStmtBody pos ident
                _ -> err ("unexpected '<' after '" <> ident <> "'")
        Just '>' -> do
            -- Check for >>> (output)
            (_, mc2) <- peek2
            case mc2 of
                Just '>' -> pIOStmtBody pos ident
                _ -> err ("unexpected '>' after '" <> ident <> "'")
        -- Qualified name output: group.member >>> channel
        Just '.' -> do
            -- Could be dotted name followed by >>> or <<<
            restParts <- many1 (try pDotIdentPart)
            let fullName = T.intercalate "." (ident : restParts)
            skipHSpace
            mc2 <- peek
            case mc2 of
                Just '>' -> pIOStmtBody pos fullName
                Just '<' -> pIOStmtBody pos fullName
                _ -> err ("expected '>>>' or '<<<' after qualified name '" <> fullName <> "'")
        _ -> err ("expected '=', '=>', '>>>', or '<<<' after identifier '" <> ident <> "'")

-- | Parse parameter set body: @=> key=val, key=val@
pParamSetBody :: SourcePos -> Identifier -> P (Located Statement)
pParamSetBody pos name = do
    char '='
    char '>'
    skipHSpace
    pairs <- pParamPairs
    skipHSpace
    skipOptionalNewline
    pure $ Located pos (ParamSet name pairs)

-- | Parse comma-separated param pairs
pParamPairs :: P [ParamPair]
pParamPairs = do
    first <- pParamPairEntry
    rest <- many (do skipWS; char ','; skipWS; pParamPairEntry)
    pure (first : rest)

-- | Parse a single param pair: @key = expr@
pParamPairEntry :: P ParamPair
pParamPairEntry = do
    key <- pIdentifier
    skipHSpace
    char '='
    skipHSpace
    ParamPair key <$> pExpr

-- | Parse binding body after @ident =@
pBindingBody :: SourcePos -> BindingPrefix -> Identifier -> P (Located Statement)
pBindingBody pos prefix ident = do
    char '='
    skipHSpace
    expr <- pExpr
    skipHSpace
    skipOptionalNewline
    pure $ Located pos (Binding prefix ident (locValue expr))

-- | Parse I/O statement body (<<< or >>>)
pIOStmtBody :: SourcePos -> Identifier -> P (Located Statement)
pIOStmtBody pos ident = do
    mc <- peek
    dir <- case mc of
        Just '<' -> do string "<<<"; pure StreamIn
        Just '>' -> do string ">>>"; pure StreamOut
        _ -> err "expected '<<<' or '>>>'"
    skipHSpace
    -- Optional channel name
    mChannel <- optional pIdentifier
    skipHSpace
    skipOptionalNewline
    pure $ Located pos (IOStmt dir ident mChannel)

-- ═══════════════════════════════════════════════════════════════════
-- Program (top-level)
-- ═══════════════════════════════════════════════════════════════════

pProgram :: P Program
pProgram = do
    -- Collect imports and statements
    (imports, stmts) <- pTopLevel [] []
    skipWS
    eof
    pure $ Program (reverse imports) (reverse stmts)

-- | Parse top-level items: imports first (can be interspersed with comments),
-- then statements.
pTopLevel ::
    [Located Import] ->
    [Located Statement] ->
    P ([Located Import], [Located Statement])
pTopLevel accImps accStmts = do
    skipWS
    mc <- peek
    case mc of
        Nothing -> pure (accImps, accStmts)
        Just '-' -> do
            -- Comment line — add to statements
            (_, mc2) <- peek2
            case mc2 of
                Just '-' -> do
                    stmt <- pCommentLine
                    pTopLevel accImps (stmt : accStmts)
                _ -> do
                    -- Negative number or something — parse as statement
                    stmt <- pStatementLine
                    pTopLevel accImps (stmt : accStmts)
        Just 'u' -> do
            -- Could be 'use' (import) or a normal identifier
            try
                ( do
                    imp <- pImportDecl
                    pTopLevel (imp : accImps) accStmts
                )
                `orElse` do
                    stmt <- pStatementLine
                    pTopLevel accImps (stmt : accStmts)
        _ -> do
            -- Parse remaining as statements
            stmts <- pStatements
            pure (accImps, reverse stmts ++ accStmts)

-- ═══════════════════════════════════════════════════════════════════
-- Public API
-- ═══════════════════════════════════════════════════════════════════

parseProgram :: FilePath -> Text -> Either ParseError Program
parseProgram path source =
    let s0 = PState source (SourcePos path 1 1)
    in  case runP pProgram s0 of
            Left e -> Left e
            Right (p, _) -> Right p

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = do
    source <- TIO.readFile path
    pure $ parseProgram path source
