-- File: src/Musikell/Language/AST.hs
-- Author: rahulmnavneeth@gmail.com
-- Docs: docs/modules/language/ast.mdx
-- Module: Musikell.Language.AST
{-# LANGUAGE DeriveFunctor #-}

module Musikell.Language.AST (
    -- * Source Location
    SourcePos (..),
    Located (..),
    Identifier,

    -- * Qualified Names
    QualName (..),

    -- * Program Structure
    Program (..),
    Import (..),
    Statement (..),
    BindingPrefix (..),
    IODirection (..),
    ParamPair (..),

    -- * Expressions
    Expr (..),
    BinOp (..),
    Literal (..),

    -- * Node Expressions
    NodeType (..),
    InputRef (..),
    NodeExpr (..),
    NamedArg (..),
    MethodCall (..),
) where

import Data.Text (Text)

-- | Source position for error reporting
data SourcePos = SourcePos
    { posFile :: !FilePath,
      posLine :: !Int,
      posColumn :: !Int
    }
    deriving (Eq, Show)

-- | A value with source location
data Located a = Located
    { locPos :: !SourcePos,
      locValue :: !a
    }
    deriving (Eq, Functor, Show)

-- | Identifier (variable name)
type Identifier = Text

-- | Dot-qualified name: @group.member@, @NS.Kernel@
newtype QualName = QualName {qualParts :: [Identifier]}
    deriving (Eq, Show)

-- | A complete program
data Program = Program
    { programImports :: ![Located Import],
      programStatements :: ![Located Statement]
    }
    deriving (Eq, Show)

-- | Import declaration
data Import
    = -- | @use "file.mkl"@
      ImportAll !FilePath
    | -- | @use "file.mkl" (X, Y)@
      ImportNames !FilePath ![Identifier]
    | -- | @use "file.mkl" as NS@
      ImportAs !FilePath !Identifier
    deriving (Eq, Show)

-- | A statement in the program
data Statement
    = -- | @[!|#] ident = expr@
      Binding !BindingPrefix !Identifier !Expr
    | -- | @ident <<< [channel]@ or @ident >>> [channel]@
      IOStmt !IODirection !Identifier !(Maybe Identifier)
    | -- | @group ident { stmts }@
      GroupDef !Identifier ![Located Statement]
    | -- | @IDENT => key=val, ...@
      ParamSet !Identifier ![ParamPair]
    | -- | @-- comment@
      CommentStmt !Text
    deriving (Eq, Show)

-- | Binding prefix for bypass/mute
data BindingPrefix
    = PrefixNone
    | -- | @!@
      PrefixBypass
    | -- | @#@
      PrefixMute
    deriving (Eq, Show)

-- | I/O direction
data IODirection
    = -- | @<<<@
      StreamIn
    | -- | @>>>@
      StreamOut
    deriving (Eq, Show)

-- | A key=value pair in a parameter set
data ParamPair = ParamPair
    { ppKey :: !Identifier,
      ppValue :: !(Located Expr)
    }
    deriving (Eq, Show)

-- | An expression
data Expr
    = -- | Literal value
      ExprLit !Literal
    | -- | Variable / qualified ref
      ExprVar !QualName
    | -- | @~qualname@ feedback ref
      ExprFeedback !QualName
    | -- | Node creation expression
      ExprNode !NodeExpr
    | -- | Binary operation
      ExprBinOp !BinOp !(Located Expr) !(Located Expr)
    | -- | Unary negation
      ExprUnaryNeg !(Located Expr)
    | -- | Parenthesized expression
      ExprParen !(Located Expr)
    | -- | Function call: @min(a, b)@
      ExprCall !Identifier ![Located Expr]
    | -- | Dot access: @expr.field@
      ExprDotAccess !(Located Expr) !Identifier
    deriving (Eq, Show)

-- | Binary operators
data BinOp
    = -- | @+@
      OpAdd
    | -- | @-@
      OpSub
    | -- | @*@
      OpMul
    | -- | @/@
      OpDiv
    deriving (Eq, Show)

-- | Literal values
data Literal
    = -- | Numeric literal
      LitNumber !Double
    | -- | String literal @"..."@
      LitString !Text
    | -- | @true@ / @false@
      LitBool !Bool
    | -- | Bare identifier in value position (e.g. @sine@, @white@)
      LitIdent !Identifier
    | -- | Beat pattern @x_o_x_o_@
      LitPattern !Text
    deriving (Eq, Show)

-- | Node type categories
data NodeType
    = -- | Generates signal
      Creational
    | -- | Transforms signal
      Manipulative
    | -- | Conditional routing
      Conditional
    deriving (Bounded, Enum, Eq, Show)

-- | An input reference (normal or feedback)
data InputRef
    = -- | Regular input
      NormalRef !QualName
    | -- | Feedback input @~node@
      FeedbackRef !QualName
    deriving (Eq, Show)

-- | A node creation expression
data NodeExpr = NodeExpr
    { -- | Node category
      neType :: !NodeType,
      -- | Input references @<a, b, ~c>@
      neInputs :: ![InputRef],
      -- | Override expressions @(AMPLITUDE * 0.9)@
      neOverrides :: ![Located Expr],
      -- | Kernel name @.Oscillator@ / @.PremVerb.Hall@
      neKernel :: !QualName,
      -- | @<AMPLITUDE>@ in @.switch<AMPLITUDE>@
      neKernelTypeParam :: !(Maybe Identifier),
      -- | Named arguments @(freq=440)@
      neArgs :: ![NamedArg],
      -- | Method chain @.switch<F>().case()@
      neMethods :: ![MethodCall]
    }
    deriving (Eq, Show)

-- | A named argument @key=expr@
data NamedArg = NamedArg
    { naName :: !Identifier,
      naValue :: !(Located Expr)
    }
    deriving (Eq, Show)

-- | A method call in a chain
data MethodCall = MethodCall
    { mcName :: !Identifier,
      -- | @<FIELD>@ in @.switch<AMPLITUDE>@
      mcTypeParam :: !(Maybe Identifier),
      mcArgs :: ![NamedArg]
    }
    deriving (Eq, Show)
