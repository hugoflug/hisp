module Eval where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent (forkIO)
import Control.Monad (when, join, void)
import Data.List (intercalate, foldl')
import qualified Data.Map as M
import Data.Map ((!?), union)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.IORef (IORef, readIORef, modifyIORef, newIORef, writeIORef)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Lang
import Parse (parse)
import System.FilePath (takeDirectory, takeFileName)
import Text.Parsec (SourcePos, sourceLine, sourceColumn, sourceName)

data SymbolValue = SymbolValue {
  value :: Value,
  imported :: Bool
} deriving (Eq, Show)

data Context = Context {
  globals :: IORef (M.Map SymbolName SymbolValue),
  locals :: M.Map String Value,
  currentDir :: String,
  currentNamespace :: IORef (Maybe String),
  stack :: [StackEntry]
}

data StackEntry = StackEntry {
  fnName :: SymbolName,
  pos :: SourcePos
} deriving (Eq, Show)

zipRestWith :: (a -> b -> c) -> [a] -> [b] -> ([c], [a], [b])
zipRestWith op (a:as) (b:bs) = 
  let (tuples, r1, r2) = zipRestWith op as bs in
    ((op a b):tuples, r1, r2)
zipRestWith _ []    bs    = ([], [], bs)
zipRestWith _ as    []     = ([], as, [])

zipRest :: [a] -> [b] -> ([(a,b)], [a], [b])
zipRest = zipRestWith (,)

pushStack :: SymbolName -> SourcePos -> [StackEntry] -> [StackEntry]
pushStack name pos stack = (StackEntry name pos):stack

orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y

mapPairs :: (Ord k, Ord k2) => (k -> v -> (k2, v2)) -> M.Map k v -> M.Map k2 v2
mapPairs fn = M.fromList . fmap (uncurry fn) . M.toList

destructure :: (Formal, Value) -> Either String [(String, Value)]
destructure (binding, value) = 
  case binding of
    SingleFormal name -> Right [(name, value)]
    DestructuringFormal names -> do
      case value of
        List vals _ -> do
          case zipRest names vals of
            (_, _:_, _) -> Left "Destructuring failed: Too few values in list"
            (_, _, _:_) -> Left "Destructuring failed: Too many values in list"
            (zipped, _, _) -> Right zipped
        x -> Left $ "Destructuring failed: Value to be destructured not a list, was [" <> printVal x <> "]" 

readSymbol :: Context -> SymbolName -> IO (Maybe Value)
readSymbol (Context globals locals _ currentNs stack) name = 
  case locals !? (localName name) of -- TODO: only check in locals if it's not fully-qualified
    Just v -> pure $ Just v
    Nothing -> do
      reg <- readIORef globals
      currNs <- readIORef currentNs
      let fullyQualified = 
            case name of
              SymbolName Nothing localName -> SymbolName currNs localName
              s -> s
      case reg !? fullyQualified `orElse` (reg !? name) of
        Just (SymbolValue v _) -> pure $ Just v
        Nothing -> case name of
          (SymbolName Nothing "true") -> pure $ Just $ Bool' True
          (SymbolName Nothing "false") -> pure $ Just $ Bool' False
          (SymbolName Nothing "nil") -> pure $ Just Nil
          s@(SymbolName Nothing localName) -> case parseBuiltin localName of
            Just builtin -> pure $ Just $ Builtin' builtin
            Nothing -> pure Nothing
          s@(SymbolName _ _ ) -> pure Nothing

eval :: Context -> Value -> IO Value
eval ctx@(Context globals locals _ currentNs stack) val =
  case val of
    Symbol name pos -> do
      let newStack = pushStack name pos stack
      maybeSymbol <- readSymbol ctx name
      case maybeSymbol of
        Just sym -> pure sym
        Nothing -> err newStack $ "No such symbol [" <> show name <> "]"

    List (head : args) pos -> do
      let fnName = case head of 
            (Symbol name _) -> name
            _ -> SymbolName Nothing "(anon)"
      let newStack = pushStack fnName pos stack
      evaledHead <- eval ctx head
      case evaledHead of
        Builtin' name -> 
          evalBuiltin ctx{stack = newStack} name args
        Function formals varArg body captures isMacro -> do
          when (length args < length formals) $ err newStack "Too few arguments"
          when (length args > length formals && isNothing varArg) $ err newStack "Too many arguments"

          args' <- if isMacro then
              pure args
            else
              traverse (eval ctx) args

          let (formalArgPairs, _, extraArgs) = zipRest formals args'
          argMap <- case traverse destructure formalArgPairs of
            Left errMsg -> err newStack errMsg
            Right destructured -> pure $ M.fromList $ join destructured
          let argMap' = case varArg of
                Just varg -> M.insert varg (List extraArgs pos) argMap -- TODO: don't set source position
                Nothing -> argMap

          let fnLocals = argMap' `union` captures
          result <- eval ctx{locals = fnLocals, stack = newStack} body
          if isMacro then
            let fnLocals = argMap' `union` locals in
              eval ctx{locals = fnLocals, stack = newStack} result
          else
            pure result

        x -> err newStack $ "No function at head of list, was: [" <> printVal x <> "]"

    v -> pure v

fmtStack :: [StackEntry] -> String
fmtStack = intercalate "\n" . fmap fmtStackEntry

fmtStackEntry :: StackEntry -> String
fmtStackEntry (StackEntry name pos) = 
  show name <> " @ " <> takeFileName (sourceName pos) <> ":" <> show (sourceLine pos) <> ":" <> show (sourceColumn pos) 

printVal :: Value -> String 
printVal v = case v of
  Int' i -> show i
  String' i -> show i
  Bool' True -> "true"
  Bool' False -> "false"
  s@(Symbol (SymbolName namespace localName) _) -> localName --TODO: fix
  List vals _ -> "(" <> (printVals " " vals) <> ")"
  fn@(Function args varArg val _ macro) -> "function" -- TODO: print it nicer
  Builtin' builtin -> show builtin -- TODO: print builtins like they are written
  Nil -> "nil"
  Custom name val -> printVal val <> "@" <> name  -- TODO: make print a method
  Ref _ -> "ref"

printVals :: String -> [Value] -> String
printVals separator = intercalate separator . fmap printVal

errNoStack :: String -> IO a
errNoStack = error

err :: [StackEntry] -> String -> IO a
err stack msg = errNoStack $ "Error: " <> msg <> " at: \n" <> fmtStack stack 

parseBuiltin :: String -> Maybe Builtin
parseBuiltin name =
  case name of
    "print" -> Just Print
    "+" -> Just Plus
    "-" -> Just Minus
    "*" -> Just Mult
    "nand" -> Just Nand
    "def" -> Just Def
    "read" -> Just Read
    "fn" -> Just Fn
    "set-macro" -> Just SetMacro
    "'" -> Just Quote
    "eval" -> Just Eval
    "apply" -> Just Apply
    "cons" -> Just Cons
    "head" -> Just Head
    "tail" -> Just Tail
    "if" -> Just If
    "=" -> Just Equals
    ">" -> Just Gt
    "split" -> Just Split
    "str" -> Just Str
    "import" -> Just Import
    "error" -> Just Error
    "read-file" -> Just Readfile
    "type" -> Just Type
    "ns" -> Just Ns
    "new" -> Just New
    "unwrap" -> Just Unwrap
    "new-symbol" -> Just NewSymbol
    "symbol-name" -> Just GetSymbolName
    "new-ref" -> Just NewRef
    "put-ref" -> Just PutRef
    "take-ref" -> Just TakeRef
    "fork" -> Just Fork
    _ -> Nothing

evalBuiltin :: Context -> Builtin -> [Value] -> IO Value
evalBuiltin ctx@(Context globals locals currDir currentNs stack) builtin args =
  case builtin of
    Print -> do
      evaledValues <- traverse (eval ctx) args
      putStrLn $ printVals " " evaledValues
      pure Nil
    Readfile -> do
      case args of
        [arg] -> do
          evaledArg <- eval ctx arg
          case evaledArg of
            String' filename -> do
              contents <- readFile filename
              pure $ String' contents
            x -> typeErr stack 1 "read-file" "string" x
        _ -> arityErr stack "read-file"
    Plus -> evalArithmetic ctx (+) "+" args
    Mult -> evalArithmetic ctx (*) "*" args
    Minus -> evalArithmetic ctx (-) "-" args
    Nand -> do
      evaledValues <- traverse (eval ctx) args
      boolArgs <- for (zip [1..] evaledValues) $ \(ix, arg) ->
            case arg of
              Bool' i -> pure i
              x -> typeErr stack ix "nand" "bool" x
      pure $ Bool' $ not $ and boolArgs
    Def ->
      case args of
        [Symbol (SymbolName Nothing localName) _, val] -> do
          evaledVal <- eval ctx val
          currNs <- readIORef currentNs
          modifyIORef globals $ \glob ->
            M.insert (SymbolName currNs localName) (SymbolValue evaledVal False) glob
          pure Nil
        [Symbol s@(SymbolName _ _) _, val] -> 
          err stack $ "Namespace not allowed in symbol passed to def: [" <> show s <> "]"
        [x, _] -> typeErr stack 1 "def" "symbol" x -- TODO: try evaluating if it's not a symbol
        _ -> arityErr stack "def"
    Read ->
      case args of
        [Symbol symbolName _] -> do
          maybeSymbol <- readSymbol ctx symbolName
          pure $ case maybeSymbol of
            Just result -> result
            Nothing -> Nil
        [x] -> typeErr stack 1 "def" "symbol" x
        _ -> arityErr stack "def"
    Fn -> evalFn stack locals args
    SetMacro ->
      case args of
        [toggle_, function_] -> do
          toggle <- eval ctx toggle_
          case toggle of
            Bool' shouldBeMacro -> do
              function <- eval ctx function_
              case function of
                fn@(Function _ _ _ _ _) -> 
                  pure fn{isMacro = shouldBeMacro}
                x -> typeErr stack 2 "set-macro" "function" x
            x -> typeErr stack 1 "set-macro" "bool" x
        _ -> arityErr stack "set-macro"
    Quote ->
      case args of
        [value] -> quote ctx value
        _ -> arityErr stack "'"
    Eval ->
      case args of
        [value] -> do
          evaledValue <- eval ctx value
          eval ctx evaledValue
        _ -> arityErr stack "eval"
    Apply ->
      case args of
        [fn, list] -> do
          evaledList <- eval ctx list
          evaledList' <- case evaledList of
            List ls _ -> pure ls
            x -> typeErr stack 2 "apply" "list" x
          let quotedList = (\v -> List [Builtin' Quote, v] (pos (head stack))) <$> evaledList'
          eval ctx (List (fn:quotedList) (pos (head stack)))
        _ -> arityErr stack "apply"
    Cons ->
      case args of
        [val, listArg] -> do
          evaledVal <- eval ctx val
          evaledList <- eval ctx listArg
          case evaledList of
            List list _ -> pure $ List (evaledVal : list) (pos (head stack))
            x -> typeErr stack 2 "cons" "list" x
        _ -> arityErr stack "cons"
    Head ->
      case args of
        [val] -> do
          evaledList <- eval ctx val
          case evaledList of
            List (head : _) _ -> pure head
            List [] _ -> err stack "head called on empty list"
            x -> typeErr stack 1 "head" "list" x
        _ -> arityErr stack "head"
    Tail ->
      case args of
        [val] -> do
          evaledList <- eval ctx val
          case evaledList of
            List (_ : tail) _ -> pure $ List tail (pos (head stack))
            List [] _ -> err stack "tail called on empty list"
            x -> typeErr stack 1 "tail" "list" x
        _ -> arityErr stack "tail"
    If ->
      case args of
        [condition, then', else'] -> do
          evaledCondition <- eval ctx condition
          case evaledCondition of
            Bool' False -> eval ctx else'
            Nil -> eval ctx else'
            _ -> eval ctx then'
        _ -> arityErr stack "if"
    Equals ->
      case args of
        [lhs, rhs] -> do
          evaledLhs <- eval ctx lhs
          evaledRhs <- eval ctx rhs
          pure $ Bool' $ evaledLhs `eq` evaledRhs
        _ -> arityErr stack "="
    Gt ->
      case args of
        [lhs, rhs] -> do
          evaledLhs <- eval ctx lhs
          evaledRhs <- eval ctx rhs
          pure $ Bool' $ case (evaledLhs, evaledRhs) of
            (Int' l, Int' r) -> l > r
            (String' l, String' r) -> l > r
            -- TODO: support > for lists
            _ -> False
        _ -> arityErr stack ">"
    Str -> do
      evaledValues <- traverse (eval ctx) args
      let 
        toString val = 
          case val of
            String' s -> s
            x -> printVal x
      pure $ String' $ intercalate "" $ toString <$> evaledValues
    Split ->
      case args of
        [arg] -> do
          evaledArg <- eval ctx arg
          case evaledArg of
            String' s -> pure $ List ((\c -> String' [c]) <$> s) (pos (head stack))
            x -> typeErr stack 1 "split" "string" x
        _ -> arityErr stack "split"
    Import ->
      case args of
        arg:rest -> do
          evaledArg <- eval ctx arg
          case evaledArg of
            String' mod -> do
              let filename = currDir <> "/" <> mod <> ".hisp"
              program <- readFile filename
              case parse filename program of
                Left e -> errNoStack $ show e
                Right exprs -> do
                  let newCurrDir = takeDirectory filename
                  newCurrNs <- newIORef Nothing
                  newGlobals <- newIORef M.empty
                  traverse_ (eval (Context newGlobals M.empty newCurrDir newCurrNs [])) exprs
                  postGlobals <- readIORef newGlobals
                  let
                    globalsToImport =
                      M.map (\v -> v{imported = True}) . M.filter (not . imported) $ postGlobals
                  modifyIORef globals $ M.union globalsToImport
                  case rest of
                    [List aliases _] -> do
                      symbolArgs <- for (zip [1..] aliases) $ \(ix, arg) ->
                        case arg of
                          Symbol (SymbolName Nothing localName) _ -> pure localName
                          Symbol (SymbolName _ localName) _ -> err stack "Fully-qualified symbol in import list"
                          x -> err stack "Non-symbol in import list"

                      for symbolArgs $ \sym ->
                        case M.size (M.filterWithKey (\k _ -> localName k == sym) globalsToImport) of
                          0 -> err stack $ "No symbol " <> sym <> " found in " <> mod
                          1 -> pure ()
                          _ -> err stack $ "Multiple symbols matching " <> sym <> " found in " <> mod

                      let 
                        symbolPred sym _ =
                          case symbolArgs of
                            [] -> True
                            symArgs -> localName sym `elem` symArgs
  
                      let aliased = mapPairs (\k v -> (k{namespace = Nothing}, v)) . M.filterWithKey symbolPred $ globalsToImport
                      modifyIORef globals $ M.union aliased
                          
                    [x] -> typeErr stack 2 "import" "list" x
                    [] -> pure ()
                    _ -> arityErr stack "import"
                  pure Nil
            x -> typeErr stack 1 "import" "string" x
        _ -> arityErr stack "import"
    Ns ->
      case args of
        [] -> do
          currNs <- readIORef currentNs
          pure $ case currNs of
            Just ns -> String' ns
            Nothing -> Nil
        [arg] -> do
          evaledArg <- eval ctx arg
          case evaledArg of
            String' ns -> do
              writeIORef currentNs (Just ns)
              pure Nil
            Nil -> do
              writeIORef currentNs Nothing
              pure Nil
            x -> typeErr stack 1 "import" "string" x
        _ -> arityErr stack "import"
    Error ->
      case args of
        [arg] -> do
          evaledArg <- eval ctx arg
          case evaledArg of
            String' msg -> do
              err stack $ msg
            x -> typeErr stack 1 "error" "string" x
        _ -> arityErr stack "error"
    Type ->
      case args of 
        [arg] -> do
          evaledArg <- eval ctx arg
          pure $ String' $ case evaledArg of
            String' _ -> "string"
            List _ _ -> "list"
            Int' _ -> "int"
            Bool' _ -> "bool"
            Symbol _ _ -> "symbol"
            Nil -> "nil"
            Builtin' _ -> "function"
            Function _ _ _ _ _ -> "function"
            Custom name _ -> name
        _ -> arityErr stack "type"
    New ->
      case args of 
        [name, arg] -> do
          evaledNameArg <- eval ctx name
          evaledArg <- eval ctx arg
          nameStr <- case evaledNameArg of
            String' n -> pure n
            x -> typeErr stack 1 "new" "string" x
          pure $ Custom nameStr evaledArg
        _ -> arityErr stack "new"
    Unwrap ->
      case args of
        [arg] -> do
          evaledArg <- eval ctx arg
          pure $ case evaledArg of
            Custom _ value -> value
            other -> other
    NewSymbol ->
      case args of
        [ns, local] -> do
          evaledNs <- eval ctx ns
          evaledLocal <- eval ctx local
          nsStr <- case evaledNs of
            String' n -> pure $ Just n
            Nil -> pure Nothing
            x -> typeErr stack 1 "new-symbol" "string" x
          localStr <- case evaledLocal of
            String' n -> pure n
            x -> typeErr stack 2 "new-symbol" "string" x
          pure $ Symbol (SymbolName nsStr localStr) (pos (head stack))
        _ -> arityErr stack "new-symbol"
    GetSymbolName ->
      case args of
        [Symbol (SymbolName ns localName) _] -> do
          let 
            nsVal = case ns of
              Nothing -> Nil
              Just name -> String' name
          pure $ List [nsVal, String' localName] (pos (head stack))
        [x] -> typeErr stack 1 "symbol-name" "symbol" x
        _ -> arityErr stack "symbol-name"
    NewRef ->
      case args of
        [] -> do
          ref <- newEmptyMVar
          pure $ Ref ref
        _ -> arityErr stack "new-ref"
    PutRef ->
      case args of
        [refArg,  val] -> do
          evaledRefArg <- eval ctx refArg
          evaledVal <- eval ctx val
          mvar <- case evaledRefArg of
            Ref mvar -> pure mvar
            x -> typeErr stack 1 "put-ref" "ref" x 
          putMVar mvar evaledVal
          pure Nil
        _ -> arityErr stack "put-ref"
    TakeRef ->
      case args of
        [refArg] -> do
          evaledRefArg <- eval ctx refArg
          mvar <- case evaledRefArg of
            Ref mvar -> pure mvar
            x -> typeErr stack 1 "take-ref" "ref" x 
          takeMVar mvar
        _ -> arityErr stack "put-ref"      
    Fork ->
      case args of
        [body] -> do
          forkIO $ void $ eval ctx body
          pure Nil
        _ -> arityErr stack "fork"   

evalArithmetic ::  Context -> (Integer -> Integer -> Integer) -> String -> [Value] -> IO Value
evalArithmetic ctx op name args = do
  evaledValues <- traverse (eval ctx) args
  intArgs <- for (zip [1..] evaledValues) $ \(ix, arg) ->
        case arg of
          Int' i -> pure i
          x -> typeErr (stack ctx) ix name "integer" x
  case intArgs of
    [] -> arityErr (stack ctx) name
    h : t -> pure $ Int' $ foldl' op (head intArgs) (tail intArgs)

eq :: Value -> Value -> Bool
eq v1 v2 = case (v1, v2) of
  (List values1 _, List values2 _) -> 
    case zipRest values1 values2 of 
      (zipped, [], []) -> all (uncurry eq) zipped
      _ -> False
  (Symbol sym1 _, Symbol sym2 _) -> sym1 == sym2
  _ -> v1 == v2

typeErr :: [StackEntry] -> Int -> String -> String -> Value -> IO a
typeErr stack argNo fnName expected actual = 
  err stack $ "Argument " <> show argNo <> " to " <> fnName <> " not of type " <> expected <> " (was " <> printVal actual <> ")"

arityErr :: [StackEntry] -> String -> IO a
arityErr stack fnName = err stack $ "Wrong number of arguments to " <> fnName

last2                    :: [a] -> Maybe (a, a)
last2 [x1, x2]           =  Just (x1, x2)
last2 (_:xs)             =  last2 xs
last2 []                 =  Nothing

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

parseFormal :: Value -> Either String Formal
parseFormal val =
  case val of
    Symbol (SymbolName Nothing name) _ -> Right $ SingleFormal name
    Symbol s@(SymbolName _ _) _ -> Left $ "Namespace in function formal: [" <> show s <> "]"
    List vals _ -> do
       names <- for vals $ \(val) ->
        case val of
          Symbol (SymbolName Nothing name) _ -> pure name
          Symbol s@(SymbolName _ _) _ -> Left $ "Namespace in function formal: [" <> show s <> "]"
          x -> Left $ "Value in destructuring list not a symbol, was [" <> printVal x <> "]" 
       Right $ DestructuringFormal names
    x -> Left $ "Function formal not a symbol or list, was [" <> printVal x <> "]" 

evalFn :: [StackEntry] -> M.Map String Value -> [Value] -> IO Value
evalFn stack captures values =
  case values of
    [List formals _, body] -> do
      maybeVarArg <- case last2 formals of
          Just (Symbol (SymbolName Nothing "&") _, lastArg) ->
            case lastArg of
              Symbol (SymbolName Nothing name) _ -> pure $ Just name
              Symbol s@(SymbolName _ _) _ -> err stack $ "Namespace in variadic function formal: [" <> show s <> "]"
              x -> err stack $ "Variadic function formal not a symbol, was: [" <> printVal x  <> "]"  
          _ -> pure Nothing
      let nonVarArgFormals = if isJust maybeVarArg then dropLast 2 formals else formals
      bindings <- case traverse parseFormal nonVarArgFormals of
        Right bindings -> pure bindings
        Left errMsg -> err stack errMsg   
      pure $ Function bindings maybeVarArg body captures False 
    [x, _] -> err stack $ "First argument to fn not a list, was: [" <> printVal x <> "]"
    _ -> err stack $ "Wrong number of arguments to fn"

quote :: Context -> Value -> IO Value
quote ctx@(Context globals locals _ _ stack) val =
  case val of
    List [Symbol (SymbolName Nothing "~") _, v] _ -> eval ctx v
    List vals pos -> do
      qVals <- traverse (quote ctx) vals
      pure $ List qVals pos
    Function formals varArg body captures macro -> do
      qBody <- quote ctx body
      pure $ Function formals varArg qBody captures macro
    v -> pure v