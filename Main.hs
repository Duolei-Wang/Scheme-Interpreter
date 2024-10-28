module Main where

import Control.Monad.Cont (liftIO)
import Control.Monad.Except (runExceptT)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.IORef
import Environment
import Evaluator (eval, evalRules, evalSyntaxClosure', makePTPair, parseEval)
import Parser
import Syntax
import Text.ParserCombinators.Parsec

instance (Show Expr) where
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
  -- printEnv globalEnv onlyName

  let isyntax = "(define-syntax if (syntax-rules () (if condition then else) (check? condition then else)))"
  let iclosure = "(syntax-rules (then else) (if condition then then-expr else else-expr) (check? condition then-expr else-expr))"

  rst <- runExceptT $ parseEval globalEnv isyntax

  rstIf <- runExceptT $ parseEval globalEnv "if"
  case rstIf of
    Left _ -> print "Error"
    Right expr -> do
      putStrLn $ showExpr expr

  let exprIfCall = parse pExpr "" "(if #t 1 2)"

  case parse pExpr "" "(if #t 1 2)" of
    Left _ -> print "parse Error"
    Right val -> do
      print $ showExpr val

  rst <- runExceptT $ parseEval globalEnv "(if #t 1 2)"
  case rst of
    Left _ -> print "Error"
    Right expr -> do
      putStrLn $ showExpr expr

  print "END"