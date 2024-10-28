module Main where

import Control.Monad.Cont (liftIO)
import Control.Monad.Except (runExceptT)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.IORef
import Environment hiding (pExprList)
import Evaluator (eval, evalRules, evalSyntaxClosure', makePTPair, parseEvalProgram)
import Parser hiding (pExprList)
import Syntax
import System.Environment (getArgs)
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
  -- args <- getArgs
  -- print args
  let onlyName = True
  globalEnv <- initEnv

  program <- readFile "test.scm"
  -- print "Program Content:"
  -- print program

  rst <- runExceptT $ parseEvalProgram globalEnv program
  case rst of
    Left err -> print err
    Right exprs -> do
      mapM_ print exprs

  print "END Program"