-- |
-- Module      : Main
-- Description : Test suite entry point
-- License     : MIT
module Main (main) where

import Test.Tasty

import qualified Test.Musikell.CLI.Parse as CLIParseTests
import qualified Test.Musikell.Core.ExecutionPlan as PlanTests
import qualified Test.Musikell.Core.Graph as GraphTests
import qualified Test.Musikell.Language.Parser as LangParserTests
import qualified Test.Musikell.Memory.Buffer as BufferTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Musikell"
        [ testGroup
            "Core"
            [ GraphTests.tests,
              PlanTests.tests
            ],
          testGroup
            "Memory"
            [ BufferTests.tests
            ],
          testGroup
            "CLI"
            [ CLIParseTests.tests
            ],
          testGroup
            "Language"
            [ LangParserTests.tests
            ]
        ]
