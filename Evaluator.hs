module Evaluator where

import Control.Monad.Except
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.IORef
import Environment (emptyEnv, initEnv)
import Parser (pExpr)
import Syntax
import Text.Parsec (parse)

parseEval :: Environment -> String -> IOThrowError Expr
parseEval env input = do
  case parse pExpr "" input of
    Left _ -> throwError $ Default "Parse Error."
    Right rst -> eval env rst

-- |
-- Evaluate the Expr until the datum-like expr. It handle the following cases: \
-- - define-syntax: it will try to resigter it to the env.\
-- - macro call: it will expanded and evaluate it.\
-- - primitive function, lambda: it also evaluate them.
eval :: Environment -> Expr -> IOThrowError Expr
eval env expr = case expr of
  Symbol s -> do
    envMap <- liftIO $ readIORef env
    case Map.lookup s envMap of
      Nothing -> throwError $ Default $ "Unbounded Symbol: " ++ s
      Just val -> return val
  List [Symbol "define-syntax", Symbol macroName, content] -> do
    closure <- evalSyntaxClosure' env macroName content
    define env macroName (Macro closure)
  List [Symbol "define", Symbol name, content] -> do
    expr' <- eval env content
    liftIO $ modifyIORef env (Map.insert name expr')
    return $ Symbol name
  List [Symbol "lambda", List params, body] -> do
    let paramNames = map (\(Symbol s) -> s) params
    return $ Lambda paramNames body env
  List (Symbol fnName : args) -> do
    fntype <- liftIO $ Map.lookup fnName <$> readIORef env
    case fntype of
      Just primfn@(Primitive pfn) -> do
        args' <- mapM (eval env) args
        pfn args'
      Just lam@(Lambda params body env) -> do
        evaluatedArgs <- mapM (eval env) args
        applyLambda env lam evaluatedArgs
      Just (Macro closure) -> applyMacro closure args
      _ -> throwError $ Default $ "Unknown symbol: " ++ fnName
  Quote expr -> return expr
  _ -> return expr

applyPrimFn :: Expr -> [Expr] -> IOThrowError Expr
applyPrimFn expr args = case expr of
  (Primitive fn) -> fn args
  _ -> throwError $ Default "Not a primitive function."

applyLambda :: Environment -> Expr -> [Expr] -> IOThrowError Expr
applyLambda env lam args = case lam of
  Lambda params body closure -> do
    if length params /= length args
      then throwError $ Default "Invalid parameters number."
      else do
        env' <- liftIO $ extendEnv params args env
        body' <- eval env' body
        eval env' body'
  _ -> do
    throwError $ Default "Invalid Lambda Expression."

extendEnv :: [String] -> [Expr] -> Environment -> IO Environment
extendEnv names exprs oldenv = do
  oldMap <- liftIO $ readIORef oldenv
  let newBindings = Map.fromList (zip names exprs)
  liftIO $ newIORef (Map.union newBindings oldMap)

applyMacro = undefined

define :: Environment -> String -> Expr -> IOThrowError Expr
define env name expr = do
  liftIO $ modifyIORef env (Map.insert name expr)
  return $ Symbol name

evalSyntaxClosure' :: Environment -> String -> Expr -> IOThrowError SyntaxClosure
evalSyntaxClosure' env macroName expr = case expr of
  List (Symbol "syntax-rules" : List literals : expr') -> do
    litNames <- mapM readSymbolName literals
    let litNameSet = Set.fromList (macroName : litNames)
    rules <- mapM (evalRule' litNameSet) expr'
    let closure = Syntax rules env
    return closure
    where
      readSymbolName :: Expr -> IOThrowError String
      readSymbolName expr = case expr of
        Symbol name -> return name
        _ -> throwError $ Default "Literals should contain only Symbols."

evalRule' :: HashSet String -> Expr -> IOThrowError Rule
evalRule' litSet expr = case expr of
  List [patExpr, temExpr] -> do
    pat <- evalPattern' litSet patExpr
    tem <- evalTemplate' (bvSet pat) temExpr
    return (pat, tem)
  _ -> throwError $ Default "Unknown syntax rule."

evalPattern' :: HashSet String -> Expr -> IOThrowError Pattern
evalPattern' litSet expr = case expr of
  Symbol name ->
    if Set.member name litSet
      then return $ PSymbol name
      else return $ PVariable name
  Number n -> return $ PSymbol (show n)
  Bool b -> return $ PSymbol (if b then "#t" else "#f")
  List [p, Symbol "..."] -> do
    pat <- evalPattern' litSet p
    return $ PRepeat pat
  List pats -> do
    pats' <- mapM (evalPattern' litSet) pats
    return $ PList pats'
  _ -> throwError $ Default "Unknown pattern define."

bvSet :: Pattern -> HashSet String
bvSet pat = case pat of
  PSymbol _ -> Set.empty
  PVariable var -> Set.singleton var
  PList lst -> Set.unions $ map bvSet lst
  PRepeat p -> bvSet p

evalTemplate' :: HashSet String -> Expr -> IOThrowError Template
evalTemplate' pvars expr = case expr of
  Symbol name ->
    if Set.member name pvars
      then return $ TVariable name
      else return $ TSymbol name
  List [t, Symbol "..."] -> do
    tem <- evalTemplate' pvars t
    return $ TRepeat tem
  List temps -> do
    temps' <- mapM (evalTemplate' pvars) temps
    return $ TList temps'