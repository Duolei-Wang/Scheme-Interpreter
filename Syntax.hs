module Syntax where

import Control.Monad.Except (ExceptT, MonadIO (liftIO))
import Data.HashMap.Strict qualified as Hash
import Data.IORef
import System.Environment (getEnv, getEnvironment)

data Expr
  = Symbol String
  | Number Integer
  | Bool Bool
  | List [Expr]
  | Quote Expr
  | Primitive PrimitiveFunction
  | Lambda [String] Expr Environment
  | Macro SyntaxClosure

typeOfExpr :: Expr -> String
typeOfExpr expr = case expr of
  Symbol s -> "<Symbol>"
  Number n -> "<Number>"
  Bool b -> "<Bool>"
  List lst -> "<List>"
  Quote expr -> "<Quote>"
  Primitive fn -> "<Primitive Function>"
  Lambda params expr env -> "<Lambda>"
  Macro closure -> "<Macro>"

showExpr :: Expr -> String
showExpr expr = case expr of
  Symbol s -> "Symbol " ++ s
  Number n -> "Number " ++ show n
  Bool b -> "Bool " ++ show b
  List lst -> "List [" ++ showaList (map showExpr lst) ++ "]"
  Quote expr' -> "Quote " ++ showExpr expr'
  Primitive fn -> "<primitive function>"
  Lambda args expr env ->
    "Lambda Args: ("
      ++ showaList (map show args)
      ++ ")\nBody: "
      ++ showExpr expr
      ++ ")"
  Macro closure -> show closure

showaList :: [String] -> String
showaList lst = case lst of
  [] -> ""
  [x] -> x
  (x : xs) -> x ++ ", " ++ showaList xs

data SyntaxClosure = SyntaxClosure [(Pattern, Template)] Environment

showSyntaxClosure :: SyntaxClosure -> String
showSyntaxClosure (SyntaxClosure rules env) = show rules

newtype SError
  = Default String
  deriving (Show, Eq)

-- Environment

type Environment = IORef (Hash.HashMap String Expr)

-- Macro

type MacroMap = Hash.HashMap (String, SyntaxClosure)

type IOThrowError = ExceptT SError IO

type PrimitiveFunction = [Expr] -> IOThrowError Expr

data Pattern
  = PSymbol String
  | PVariable String
  | PList [Pattern]
  | PRepeat Pattern

instance Show Pattern where
  show :: Pattern -> String
  show = showPattern

showPattern :: Pattern -> String
showPattern p = case p of
  PSymbol s -> "PSymbol " ++ show s
  PVariable v -> "PVariable " ++ show v
  PList lst -> "PList [" ++ unwords (map showPattern lst) ++ "]"
  PRepeat p -> "PRepeat " ++ showPattern p

data Template
  = TSymbol String
  | TVariable String
  | TList [Template]
  | TRepeat Template

instance Show Template where
  show :: Template -> String
  show = showTemplate

showTemplate :: Template -> String
showTemplate t = case t of
  TSymbol s -> "TSymbol " ++ show s
  TVariable v -> "TVariable " ++ show v
  TList lst -> "TList [" ++ unwords (map showTemplate lst) ++ "]"
  TRepeat p -> "TRepeat " ++ showTemplate p

type Rule = (Pattern, Template)

instance (Show SyntaxClosure) where
  show :: SyntaxClosure -> String
  show (SyntaxClosure rules env) = show rules