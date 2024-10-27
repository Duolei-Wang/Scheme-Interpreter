module Main where

import Control.Monad.Cont (liftIO)
import Control.Monad.Except (runExceptT)
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Environment
import Evaluator (evalSyntaxClosure', parseEval)
import Parser
import Syntax
import Text.ParserCombinators.Parsec

instance Show Expr where
  show :: Expr -> String
  show = showExpr

easyCheck :: Expr -> Either ParseError Expr
easyCheck expr = case expr of
  List [Symbol "syntax-rules", literals, expr'] -> do
    return $ Symbol "Yes"
  List (Symbol "lambda" : args : expr') -> do
    return $ Symbol "Yes"

main :: IO ()
main = do
  let onlyName = True
  globalEnv <- initEnv
  printEnv globalEnv onlyName

  let icheck = "(define x `(1 2 3))"

  printEnv globalEnv onlyName
  runExceptT $ parseEval globalEnv icheck
  printEnv globalEnv onlyName

  rst <- runExceptT $ parseEval globalEnv "(check? #t 1 2)"
  case rst of
    Left _ -> print "No"
    Right val -> print val

  let isyntax = "(define-syntax if (syntax-rules (then else) ( (if condition then then-expr else else-expr) (check? condition then-expr else-expr))))"
  rst <- runExceptT $ parseEval globalEnv isyntax
  case rst of
    Left _ -> print "No"
    Right val -> print val

  let iif = "(if #t then 1 else 2)"
  rst <- runExceptT $ parseEval globalEnv iif
  case rst of
    Left _ -> print "No"
    Right val -> print val

-- printEnv globalEnv
-- printEnv globalEnv

-- let icall = "(lam1 1 2)"
-- rst <- runExceptT $ parseEval globalEnv icall
-- case rst of
--   Left _ -> print "No"
--   Right val -> print val
