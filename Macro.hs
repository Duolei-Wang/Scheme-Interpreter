module Macro where

import Control.Monad.Except
import Data.HashMap.Strict qualified as Map
import Data.IORef
import Syntax

applyMacro :: Environment -> SyntaxClosure -> Expr -> IOThrowError Expr
applyMacro env (SyntaxClosure rules envc) macroCall = do
  (subsmap, template) <- tryRules rules macroCall
  envMap <- liftIO $ readIORef env
  envcMap <- liftIO $ readIORef envc
  let finalEnvMap = Map.union envcMap envMap
  finalEnv <- liftIO $ newIORef finalEnvMap
  subsTemplate subsmap template

-- |
-- Substitute the variable in a template with the Expr in env, return the replaced Expr.\\
-- And only the TRepeat has a list of expr to be substituted.\\
-- Other's result is just a singleton list [Expr] in Symbol -> [Expr].
subsTemplate :: Map.HashMap String [Expr] -> Template -> IOThrowError Expr
subsTemplate smap template = case template of
  TSymbol ts -> return $ Symbol ts
  TVariable tv -> do
    case Map.lookup tv smap of
      Nothing -> throwError $ Default $ "Unbounded Template Variable: " ++ tv
      Just (expr : _) -> return expr
  TList tlst -> do
    rest <- mapM (subsTemplate smap) tlst
    return $ List rest
  TRepeat t -> do
    rst <- replicateTemplate smap t
    return $ List rst
    where
      replicateTemplate :: Map.HashMap String [Expr] -> Template -> IOThrowError [Expr]
      replicateTemplate smap t = case t of
        TVariable tv -> do
          case Map.lookup tv smap of
            Nothing -> throwError $ Default "Unbounded Template Variable"
            Just expr -> return expr
        template -> do
          rst <- subsTemplate smap template
          return [rst]

tryRules :: [(Pattern, Template)] -> Expr -> IOThrowError (Map.HashMap String [Expr], Template)
tryRules rules args = case rules of
  [] -> throwError $ Default "No matching pattern."
  ((pat, tem) : rest) -> do
    let subsMap = matchPattern (pat, args)
    case subsMap of
      Nothing -> tryRules rest args
      Just smap -> return (smap, tem)

-- |
-- Match pattern with given expr one-by-one.\\
-- The first matched pattern will be used to build the substitute map.\\
-- The substitute map build a Map(String, Expr), will be used in template.\
-- When we match patterns, we actually try to find which Variables will be replaced by Expr.
matchPattern :: (Pattern, Expr) -> Maybe (Map.HashMap String [Expr])
matchPattern (pat, expr) = case (pat, expr) of
  (PSymbol ps, Symbol s) ->
    if ps /= s
      then Nothing
      else Just Map.empty
  (PVariable pv, expr) -> Just $ Map.singleton pv [expr]
  (PList [], List []) -> Just Map.empty
  (PList lstpat, List lstexpr) ->
    if length lstpat /= length lstexpr
      then Nothing
      else do
        let pairs = zip lstpat lstexpr
        matches <- mapM matchPattern pairs
        Just $ Map.unions matches -- If there is something Nothing in matches, the result is also Nothing.
  (PRepeat p, List lstexpr) -> matchRepeatP p lstexpr
  _ -> Nothing

-- |
-- Notice that, when we meet the repeat pattern, the matched expr will be packed in a List.\\
-- e.g. PRepeat (Psymbol "x") meets [Symbol "a", Symbol "b"], \\
-- we will build a result like "x" -> List [Symbol "a", Symbol "b"].\\
-- Thus when we expand a template, we should to unpack the result there.
matchRepeatP :: Pattern -> [Expr] -> Maybe (Map.HashMap String [Expr])
matchRepeatP p expr = case expr of
  [] -> Just Map.empty
  (e : es) -> do
    fstmatch <- matchPattern (p, e)
    restmatch <- matchRepeatP p es
    return $ combinePatterns fstmatch restmatch
    where
      joinExpr :: Expr -> Expr -> Expr
      joinExpr lhs rhs = case (lhs, rhs) of
        (List lexprs, List rexprs) -> List $ lexprs ++ rexprs
        (List lexprs, rhs) -> List $ rhs : lexprs
        (lhs, List rexprs) -> List $ lhs : rexprs
        (lhs, rhs) -> List [lhs, rhs]

combinePatterns :: Map.HashMap String [Expr] -> Map.HashMap String [Expr] -> Map.HashMap String [Expr]
combinePatterns = Map.unionWith (++)