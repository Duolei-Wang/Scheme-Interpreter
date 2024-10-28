# Scheme-Interpreter

I'm learnig Write Your Scheme Interpreter in 48h, A Haskell Tutorial. Here is the archive of my codes. I'm not sure whether it will finally meets some standard of Scheme, just for learning PLT.

Here is no optimization and efficiency I guess XD. And after I test it's robustness, I will fill the README.md more to make it an explaination of the Tutorial.

## Progress
- 10.28
  I fixed the macro apply system and test easy case (without replicate macro rules). 

- 10.27
  Today, I make most of my interpreter. The framework contains an unfinished Macro system. Right now, we can define macro and transformer properly but the applyMacro is still undefined.
 
## 1. `Syntax.hs`

```haskell
-- Syntax.hs
data Expr
  = Symbol String
  | Number Integer
  | List [Expr]
  | Primitive PrimitiveFunction
  | Lambda [String] Expr Environment
  | Macro SyntaxClosure


data SyntaxClosure = Syntax [(Pattern, Template)] Environment

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

data Template
  = TSymbol String
  | TVariable String
  | TList [Template]
  | TRepeat Template

type Rule = (Pattern, Template)
```

## 2. `Evaluator.hs`

The core of my evaluator is the eval. It receive an environment and an expr to calculate the expr's value. Here, the value means the expr can't be move anymore. As I provide the define-syntax function, so I need to consider the Macro case.

```haskell
-- Evaluator.hs
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
  _ -> return expr
```

