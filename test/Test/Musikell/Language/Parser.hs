{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.Musikell.Language.Parser
-- Description : Tests for the v2 parser
-- License     : MIT
module Test.Musikell.Language.Parser (tests) where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T

import Musikell.Language.AST
import Musikell.Language.Parser

tests :: TestTree
tests =
    testGroup
        "Language.Parser"
        [ importTests,
          bindingTests,
          ioTests,
          expressionTests,
          nodeExprTests,
          groupTests,
          paramSetTests,
          commentTests,
          fullProgramTests,
          errorTests
        ]

-- ═══════════════════════════════════════════════════════════════════
-- Helpers
-- ═══════════════════════════════════════════════════════════════════

-- | Parse a program and return the result
parse :: Text -> Either ParseError Program
parse = parseProgram "<test>"

-- | Parse and expect success, returning the program
parseOk :: Text -> Program
parseOk src = case parse src of
    Left e -> error $ "parse failed: " ++ show e
    Right p -> p

-- | Get the first statement from a successful parse
firstStmt :: Text -> Statement
firstStmt src = case programStatements (parseOk src) of
    (Located _ s : _) -> s
    [] -> error "no statements"

-- | Get the first import from a successful parse
firstImport :: Text -> Import
firstImport src = case programImports (parseOk src) of
    (Located _ i : _) -> i
    [] -> error "no imports"

-- | Get the expression from the first binding
firstBindingExpr :: Text -> Expr
firstBindingExpr src = case firstStmt src of
    Binding _ _ e -> e
    other -> error $ "expected binding, got: " ++ show other

-- ═══════════════════════════════════════════════════════════════════
-- Import tests
-- ═══════════════════════════════════════════════════════════════════

importTests :: TestTree
importTests =
    testGroup
        "Imports"
        [ testCase "bare import" $ do
            let imp = firstImport "use \"./foo.mkl\"\n"
            case imp of
                ImportAll path -> path @?= "./foo.mkl"
                other -> assertFailure $ "expected ImportAll, got: " ++ show other,
          testCase "selective import" $ do
            let imp = firstImport "use \"./kit.mkl\" (HouseKick, HouseHat)\n"
            case imp of
                ImportNames path names -> do
                    path @?= "./kit.mkl"
                    names @?= ["HouseKick", "HouseHat"]
                other -> assertFailure $ "expected ImportNames, got: " ++ show other,
          testCase "namespaced import" $ do
            let imp = firstImport "use \"./verbs.mkl\" as PremVerb\n"
            case imp of
                ImportAs path ns -> do
                    path @?= "./verbs.mkl"
                    ns @?= "PremVerb"
                other -> assertFailure $ "expected ImportAs, got: " ++ show other
        ]

-- ═══════════════════════════════════════════════════════════════════
-- Binding tests
-- ═══════════════════════════════════════════════════════════════════

bindingTests :: TestTree
bindingTests =
    testGroup
        "Bindings"
        [ testCase "simple variable binding" $ do
            let s = firstStmt "x = y\n"
            case s of
                Binding PrefixNone "x" (ExprVar (QualName ["y"])) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "bypass prefix" $ do
            let s = firstStmt "!x = y\n"
            case s of
                Binding PrefixBypass "x" (ExprVar (QualName ["y"])) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "mute prefix" $ do
            let s = firstStmt "#x = y\n"
            case s of
                Binding PrefixMute "x" (ExprVar (QualName ["y"])) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "binding to node expression" $ do
            let s = firstStmt "osc = Creational(0.5).Oscillator(freq=440.0)\n"
            case s of
                Binding PrefixNone "osc" (ExprNode ne) -> do
                    neType ne @?= Creational
                    resolveQN (neKernel ne) @?= "Oscillator"
                other -> assertFailure $ "unexpected: " ++ show other
        ]

-- ═══════════════════════════════════════════════════════════════════
-- I/O tests
-- ═══════════════════════════════════════════════════════════════════

ioTests :: TestTree
ioTests =
    testGroup
        "I/O"
        [ testCase "input with channel" $ do
            let s = firstStmt "vocal_in <<< vocal\n"
            case s of
                IOStmt StreamIn "vocal_in" (Just "vocal") -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "input without channel" $ do
            let s = firstStmt "mic <<<\n"
            case s of
                IOStmt StreamIn "mic" Nothing -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "output with channel" $ do
            let s = firstStmt "x = y\nx >>> speakers\n"
            let prog = parseOk "x = y\nx >>> speakers\n"
            case programStatements prog of
                [_, Located _ (IOStmt StreamOut "x" (Just "speakers"))] -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "qualified name output" $ do
            let prog = parseOk "x = y\nmaster.lim >>> main_speakers\n"
            case programStatements prog of
                [_, Located _ (IOStmt StreamOut "master.lim" (Just "main_speakers"))] -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other
        ]

-- ═══════════════════════════════════════════════════════════════════
-- Expression tests
-- ═══════════════════════════════════════════════════════════════════

expressionTests :: TestTree
expressionTests =
    testGroup
        "Expressions"
        [ testCase "number literal" $ do
            case firstBindingExpr "x = 42.0\n" of
                ExprLit (LitNumber 42.0) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "integer literal" $ do
            case firstBindingExpr "x = 440\n" of
                ExprLit (LitNumber 440.0) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "string literal" $ do
            case firstBindingExpr "x = \"hello\"\n" of
                ExprLit (LitString "hello") -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "boolean true" $ do
            case firstBindingExpr "x = true\n" of
                ExprLit (LitBool True) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "boolean false" $ do
            case firstBindingExpr "x = false\n" of
                ExprLit (LitBool False) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "variable reference" $ do
            case firstBindingExpr "x = foo\n" of
                ExprVar (QualName ["foo"]) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "qualified name" $ do
            case firstBindingExpr "x = vocal_chain.hp\n" of
                ExprVar (QualName ["vocal_chain", "hp"]) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "addition" $ do
            case firstBindingExpr "x = 1 + 2\n" of
                ExprBinOp OpAdd _ _ -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "multiplication" $ do
            case firstBindingExpr "x = 3 * 4\n" of
                ExprBinOp OpMul _ _ -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "precedence: mul before add" $ do
            -- 1 + 2 * 3 should parse as 1 + (2 * 3)
            case firstBindingExpr "x = 1 + 2 * 3\n" of
                ExprBinOp
                    OpAdd
                    (Located _ (ExprLit (LitNumber 1.0)))
                    (Located _ (ExprBinOp OpMul _ _)) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "parenthesized expression" $ do
            case firstBindingExpr "x = (1 + 2) * 3\n" of
                ExprBinOp OpMul (Located _ (ExprParen _)) _ -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "division" $ do
            case firstBindingExpr "x = 10 / 2\n" of
                ExprBinOp OpDiv _ _ -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "unary negation" $ do
            case firstBindingExpr "x = -1\n" of
                ExprUnaryNeg (Located _ (ExprLit (LitNumber 1.0))) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "function call min" $ do
            case firstBindingExpr "x = min(1, 2)\n" of
                ExprCall "min" [_, _] -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "function call max" $ do
            case firstBindingExpr "x = max(a, b)\n" of
                ExprCall "max" [_, _] -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "function call clamp" $ do
            case firstBindingExpr "x = clamp(v, 0, 1)\n" of
                ExprCall "clamp" [_, _, _] -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "feedback reference" $ do
            case firstBindingExpr "x = ~mynode\n" of
                ExprFeedback (QualName ["mynode"]) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "feedback qualified reference" $ do
            case firstBindingExpr "x = ~vocal_chain.delay\n" of
                ExprFeedback (QualName ["vocal_chain", "delay"]) -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other
        ]

-- ═══════════════════════════════════════════════════════════════════
-- Node expression tests
-- ═══════════════════════════════════════════════════════════════════

nodeExprTests :: TestTree
nodeExprTests =
    testGroup
        "Node Expressions"
        [ testCase "creational with no input" $ do
            case firstBindingExpr "x = Creational(0.5).Oscillator(freq=440.0)\n" of
                ExprNode ne -> do
                    neType ne @?= Creational
                    neInputs ne @?= []
                    length (neOverrides ne) @?= 1
                    resolveQN (neKernel ne) @?= "Oscillator"
                    length (neArgs ne) @?= 1
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "manipulative with single input" $ do
            case firstBindingExpr "x = Manipulative<vocal_in>().Reverb(wet=0.3)\n" of
                ExprNode ne -> do
                    neType ne @?= Manipulative
                    length (neInputs ne) @?= 1
                    case head (neInputs ne) of
                        NormalRef (QualName ["vocal_in"]) -> pure ()
                        other -> assertFailure $ "unexpected input ref: " ++ show other
                    resolveQN (neKernel ne) @?= "Reverb"
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "multi-input with feedback" $ do
            case firstBindingExpr "x = Manipulative<reverb, ~delay>().Delay(samples=4410)\n" of
                ExprNode ne -> do
                    length (neInputs ne) @?= 2
                    case neInputs ne of
                        [NormalRef (QualName ["reverb"]), FeedbackRef (QualName ["delay"])] -> pure ()
                        other -> assertFailure $ "unexpected inputs: " ++ show other
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "override expression" $ do
            case firstBindingExpr "x = Manipulative<a>(AMPLITUDE * 0.9).Gain(level=1.0)\n" of
                ExprNode ne -> do
                    length (neOverrides ne) @?= 1
                    case locValue (head (neOverrides ne)) of
                        ExprBinOp OpMul _ _ -> pure ()
                        other -> assertFailure $ "unexpected override: " ++ show other
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "named arguments" $ do
            case firstBindingExpr "x = Creational(1.0).Oscillator(freq=440.0, waveform=sine)\n" of
                ExprNode ne -> do
                    length (neArgs ne) @?= 2
                    naName (head (neArgs ne)) @?= "freq"
                    naName (neArgs ne !! 1) @?= "waveform"
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "method chain" $ do
            let src = "x = Conditional<input>().switch<AMPLITUDE>(default=default_node).case(lo=0.0, hi=0.05, target=low_node)\n"
            case firstBindingExpr src of
                ExprNode ne -> do
                    neType ne @?= Conditional
                    resolveQN (neKernel ne) @?= "switch"
                    neKernelTypeParam ne @?= Just "AMPLITUDE"
                    length (neMethods ne) @?= 1
                    let m = head (neMethods ne)
                    mcName m @?= "case"
                    length (mcArgs m) @?= 3
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "method with type parameter" $ do
            let src = "x = Conditional<input>().switch<FREQUENCY>(default=default_node)\n"
            case firstBindingExpr src of
                ExprNode ne -> do
                    resolveQN (neKernel ne) @?= "switch"
                    neKernelTypeParam ne @?= Just "FREQUENCY"
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "namespaced kernel" $ do
            case firstBindingExpr "x = Manipulative<a>().PremVerb.Hall(size=0.9)\n" of
                ExprNode ne -> do
                    resolveQN (neKernel ne) @?= "PremVerb.Hall"
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "beat pattern" $ do
            case firstBindingExpr "x = Manipulative<kick>().Beat(pattern=x___x___x___x___)\n" of
                ExprNode ne -> do
                    resolveQN (neKernel ne) @?= "Beat"
                    case neArgs ne of
                        [NamedArg "pattern" (Located _ (ExprLit (LitPattern pat)))] ->
                            pat @?= "x___x___x___x___"
                        other -> assertFailure $ "unexpected args: " ++ show other
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "empty args" $ do
            case firstBindingExpr "x = Manipulative<a>().Mixer()\n" of
                ExprNode ne -> do
                    neArgs ne @?= []
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "conditional node type" $ do
            case firstBindingExpr "x = Conditional<a>().switch<AMPLITUDE>(default=b)\n" of
                ExprNode ne -> do
                    neType ne @?= Conditional
                    neKernelTypeParam ne @?= Just "AMPLITUDE"
                other -> assertFailure $ "unexpected: " ++ show other
        ]

-- ═══════════════════════════════════════════════════════════════════
-- Group tests
-- ═══════════════════════════════════════════════════════════════════

groupTests :: TestTree
groupTests =
    testGroup
        "Groups"
        [ testCase "simple group" $ do
            let s = firstStmt "group gen {\n  x = y\n}\n"
            case s of
                GroupDef "gen" stmts -> length stmts @?= 1
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "nested group" $ do
            let src = "group outer {\n  group inner {\n    x = y\n  }\n}\n"
            let s = firstStmt src
            case s of
                GroupDef "outer" [Located _ (GroupDef "inner" [Located _ (Binding _ "x" _)])] -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "group with multiple statements" $ do
            let src = "group g {\n  a = b\n  c = d\n}\n"
            let s = firstStmt src
            case s of
                GroupDef "g" stmts -> length stmts @?= 2
                other -> assertFailure $ "unexpected: " ++ show other
        ]

-- ═══════════════════════════════════════════════════════════════════
-- Parameter set tests
-- ═══════════════════════════════════════════════════════════════════

paramSetTests :: TestTree
paramSetTests =
    testGroup
        "Parameter Sets"
        [ testCase "simple param set" $ do
            let s = firstStmt "VENUE => room_size = 0.85, decay = 2.4\n"
            case s of
                ParamSet "VENUE" pairs -> do
                    length pairs @?= 2
                    ppKey (head pairs) @?= "room_size"
                    ppKey (pairs !! 1) @?= "decay"
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "param set with expressions" $ do
            let s = firstStmt "CLOCK => bpm = 128, division = 16\n"
            case s of
                ParamSet "CLOCK" pairs -> do
                    length pairs @?= 2
                    ppKey (head pairs) @?= "bpm"
                other -> assertFailure $ "unexpected: " ++ show other
        ]

-- ═══════════════════════════════════════════════════════════════════
-- Comment tests
-- ═══════════════════════════════════════════════════════════════════

commentTests :: TestTree
commentTests =
    testGroup
        "Comments"
        [ testCase "simple comment" $ do
            let s = firstStmt "-- this is a comment\n"
            case s of
                CommentStmt txt -> T.strip txt @?= "this is a comment"
                other -> assertFailure $ "unexpected: " ++ show other,
          testCase "empty comment" $ do
            let s = firstStmt "--\n"
            case s of
                CommentStmt _ -> pure ()
                other -> assertFailure $ "unexpected: " ++ show other
        ]

-- ═══════════════════════════════════════════════════════════════════
-- Full program tests
-- ═══════════════════════════════════════════════════════════════════

fullProgramTests :: TestTree
fullProgramTests =
    testGroup
        "Full Programs"
        [ testCase "imports then statements" $ do
            let src =
                    T.unlines
                        [ "use \"./kit.mkl\" (Kick)",
                          "use \"./verb.mkl\" as PV",
                          "CLOCK => bpm = 128",
                          "input <<< mic",
                          "x = Creational(1.0).Oscillator(freq=440.0)",
                          "x >>> speakers"
                        ]
            let prog = parseOk src
            length (programImports prog) @?= 2
            -- statements: CLOCK param set + input + binding + output
            length (programStatements prog) @?= 4,
          testCase "empty program" $ do
            let prog = parseOk ""
            programImports prog @?= []
            programStatements prog @?= [],
          testCase "program with only comments" $ do
            let src = "-- comment 1\n-- comment 2\n"
            let prog = parseOk src
            length (programStatements prog) @?= 2
        ]

-- ═══════════════════════════════════════════════════════════════════
-- Error tests
-- ═══════════════════════════════════════════════════════════════════

errorTests :: TestTree
errorTests =
    testGroup
        "Error Cases"
        [ testCase "unterminated string" $ do
            case parse "x = \"hello\n" of
                Left _ -> pure ()
                Right _ -> assertFailure "should fail on unterminated string",
          testCase "unknown node type" $ do
            case parse "x = Unknown<a>().Foo()\n" of
                Left _ -> pure ()
                Right _ -> assertFailure "should fail on unknown node type"
        ]

-- ═══════════════════════════════════════════════════════════════════
-- Helpers
-- ═══════════════════════════════════════════════════════════════════

resolveQN :: QualName -> Text
resolveQN (QualName parts) = T.intercalate "." parts
