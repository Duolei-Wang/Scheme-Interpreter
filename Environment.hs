module Environment where

import Control.Monad.Except (throwError)
import Data.HashMap.Strict as Map (empty, fromList, insert, toList)
import Data.IORef
import Syntax

emptyEnv :: IO Environment
emptyEnv = newIORef Map.empty

initEnv :: IO Environment
initEnv = initEnv' pfnList pExprList

initEnv' :: [(String, PrimitiveFunction)] -> [(String, Expr)] -> IO Environment
initEnv' pfLst pexprLst = do
  env <- emptyEnv
  mapM_ (uncurry $ registerPFn env) pfLst
  mapM_ (uncurry $ registerPExpr env) pexprLst
  return env
  where
    registerPFn :: Environment -> String -> PrimitiveFunction -> IO ()
    registerPFn env name pfn = do
      let pfexpr = Primitive pfn
      modifyIORef env (Map.insert name pfexpr)

    registerPExpr :: Environment -> String -> Expr -> IO ()
    registerPExpr env name expr = do
      modifyIORef env (Map.insert name expr)

showEnv :: Environment -> Bool -> IO [String]
showEnv env onlyName = do
  envMap <- readIORef env
  let lst = Map.toList envMap
  return $ map (showPair onlyName) lst
  where
    showPair :: Bool -> (String, Expr) -> String
    showPair flag (str, expr) =
      if flag
        then "Symbol Name:\n\t" ++ str
        else "Symbol Name:\n\t" ++ str ++ "\nSymbol Type:\n\t" ++ typeOfExpr expr ++ "\nSymbol Content:\n" ++ showExpr expr ++ ")\n"

printEnv :: Environment -> Bool -> IO ()
printEnv env onlyName = do
  envContent <- showEnv env onlyName
  putStrLn "<environment head>"
  if null envContent
    then putStrLn "Empty Environment"
    else do
      mapM_ putStrLn envContent
  putStrLn "<environment tail>"

pExprList :: [(String, Expr)]
pExprList =
  [ ("#t", Bool True),
    ("#f", Bool False)
  ]

pfnList :: [(String, PrimitiveFunction)]
pfnList =
  [ ("+", pAdd),
    ("-", pSub),
    ("*", pMul),
    ("/", pDiv),
    ("car", car),
    ("cdr", cdr),
    ("list?", isList),
    ("null?", nullList),
    ("eq?", eq),
    ("check?", pcheck)
  ]

car :: PrimitiveFunction
car expr = case expr of
  [List (x : xs)] -> do
    return x
  _ -> throwError $ Default "car can't act on non-list data."

cdr :: PrimitiveFunction
cdr expr = case expr of
  [List (x : xs)] -> return $ List xs
  _ -> throwError $ Default "car can't act on non-list data."

isList :: PrimitiveFunction
isList expr = case expr of
  [List lst] -> do
    return $ Bool True
  _ -> return $ Bool False

nullList :: PrimitiveFunction
nullList expr = case expr of
  [List []] -> return $ Bool True
  _ -> return $ Bool False

-- |
-- eq? of datum. One should define their own eq in scheme.
eq :: PrimitiveFunction
eq args = case args of
  [Number a, Number b] -> do
    if a == b
      then return $ Bool True
      else return $ Bool False
  [Bool a, Bool b] -> do
    if a == b
      then return $ Bool True
      else return $ Bool False
  _ -> throwError $ Default "Number args error."

pcheck :: PrimitiveFunction
pcheck args = case args of
  [Bool b, yes, no] -> do
    if b
      then return yes
      else return no

pAdd :: PrimitiveFunction
pAdd args = case args of
  [] -> throwError $ Default "Number args error."
  [Number a] -> throwError $ Default "Number args error."
  [Number a, Number b] -> do
    return $ Number $ (+) a b
  (Number lhs : rhs) -> do
    rexpr <- pAdd rhs
    case rexpr of
      Number rval -> return $ Number $ (+) lhs rval
      _ -> throwError $ Default "Function acts on Numbers."
  _ -> throwError $ Default "Default error."

pSub :: PrimitiveFunction
pSub args = case args of
  [] -> throwError $ Default "Number args error."
  [Number a] -> throwError $ Default "Number args error."
  [Number a, Number b] -> do
    return $ Number $ (-) a b
  (Number lhs : rhs) -> do
    rexpr <- pAdd rhs
    case rexpr of
      Number rval -> return $ Number $ (-) lhs rval
      _ -> throwError $ Default "Function acts on Numbers."

pMul :: PrimitiveFunction
pMul args = case args of
  [] -> throwError $ Default "Number args error."
  [Number a] -> throwError $ Default "Number args error."
  [Number a, Number b] -> do
    return $ Number $ (*) a b
  (Number lhs : rhs) -> do
    rexpr <- pAdd rhs
    case rexpr of
      Number rval -> return $ Number $ (*) lhs rval
      _ -> throwError $ Default "Function acts on Numbers."

pDiv :: PrimitiveFunction
pDiv args = case args of
  [] -> throwError $ Default "Number args error."
  [Number a] -> throwError $ Default "Number args error."
  [Number a, Number b] -> do
    if b == 0
      then throwError $ Default "Divided by zero."
      else return $ Number $ div a b
  (Number lhs : rhs) -> do
    rexpr <- pAdd rhs
    case rexpr of
      Number rval -> return $ Number $ div lhs rval
      _ -> throwError $ Default "Function acts on Numbers."
