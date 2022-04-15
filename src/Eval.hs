module Eval where

import Control.Monad (when)
import Data.List (intercalate, foldl')
import qualified Data.Map as M
import Data.Map ((!?), union)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Lang
import Parse (parse)
import System.FilePath (takeDirectory, takeFileName)
import Text.Parsec (SourcePos, sourceLine, sourceColumn, sourceName)

-- Refer to previous lets inside a let binding
-- CLI
-- Better REPL (repline?)
-- Macros evaluated at load time
-- Destructuring?

data Context = Context {
  globals :: IORef (M.Map String Value),
  locals :: M.Map String Value,
  currentDir :: String,
  stack :: [StackEntry]
}

data StackEntry = StackEntry {
  fnName :: String,
  pos :: SourcePos
} deriving (Eq, Show)

zipRest :: [a] -> [b] -> ([(a,b)], [a], [b])
zipRest (a:as) (b:bs) = 
  let (tuples, r1, r2) = zipRest as bs in
    ((a, b):tuples, r1, r2)
zipRest []     bs    = ([], [], bs)
zipRest as    []     = ([], as, [])

pushStack :: String -> SourcePos -> [StackEntry] -> [StackEntry]
pushStack name pos stack = (StackEntry name pos):stack

eval :: Context -> Value -> IO Value
eval ctx@(Context globals locals _ stack) val =
  case val of
    Symbol name pos -> do
      let newStack = pushStack name pos stack
      case locals !? name of
        Just v -> pure v
        Nothing -> do
          reg <- readIORef globals
          case reg !? name of
            Just v -> pure v
            Nothing -> case name of
              "true" -> pure $ Bool' True
              "false" -> pure $ Bool' False
              "nil" -> pure Nil
              name -> case parseBuiltin name of
                Just builtin -> pure $ Builtin' builtin
                Nothing -> do
                  err newStack $ "No such symbol [" <> name <> "]"

    List (head : tail) pos -> do
      let 
        fnName = 
          case head of 
            (Symbol name _) -> name
            _ -> "(anon)"
      let newStack = pushStack fnName pos stack
      evaledHead <- eval ctx head
      case evaledHead of
        Builtin' name -> 
          evalBuiltin ctx{stack = newStack} name tail
        Function args varArg body captures isMacro -> do
          when (length tail < length args) $ err newStack "Too few arguments"
          when (length tail > length args && isNothing varArg) $ err newStack "Too many arguments"

          fnTail <- if isMacro then
              pure tail
            else
              traverse (eval ctx) tail

          argMap <- M.fromList <$> case zipRest args fnTail of
            (zipped, _, extras) -> do
              case varArg of
                Just varg -> pure $ zipped <> [(varg, List extras pos)] -- TODO: don't set source position
                Nothing -> pure zipped

          let fnLocals = argMap `union` captures
          result <- eval ctx{locals = fnLocals, stack = newStack} body
          if isMacro then
            let fnLocals = argMap `union` locals in
              eval ctx{locals = fnLocals, stack = newStack} result
          else
            pure result

        x -> err newStack $ "No function at head of list, was: [" <> printVal x <> "]"

    v -> pure v

fmtStack :: [StackEntry] -> String
fmtStack = intercalate "\n" . fmap fmtStackEntry

fmtStackEntry :: StackEntry -> String
fmtStackEntry (StackEntry fnName pos) = 
  fnName <> " @ " <> takeFileName (sourceName pos) <> ":" <> show (sourceLine pos) <> ":" <> show (sourceColumn pos) 

printVal :: Value -> String 
printVal v = case v of
  Int' i -> show i
  String' i -> show i
  Bool' True -> "true"
  Bool' False -> "false"
  Symbol name _ -> "#" <> name
  List vals _ -> "(" <> (printVals " " vals) <> ")"
  Function args varArg val _ macro -> "function" -- TODO: print it nicer
  Builtin' builtin -> show builtin -- TODO: print builtins like they are written
  Nil -> "nil"

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
    _ -> Nothing

evalBuiltin :: Context -> Builtin -> [Value] -> IO Value
evalBuiltin ctx@(Context globals locals currDir stack) builtin args =
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
              x -> typeErr stack ix "-" "bool" x
      pure $ Bool' $ not $ and boolArgs
    Def ->
      case args of
        [Symbol name _, val] -> do
          evaledVal <- eval ctx val
          modifyIORef globals $ \glob ->
            M.insert name evaledVal glob
          pure Nil
        [x, _] -> typeErr stack 1 "def" "symbol" x
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
                x -> typeErr stack 2 "set-macro" "bool" x
            x -> typeErr stack 1 "set-macro" "function" x
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
          eval ctx (List (fn:evaledList') (pos (head stack)))
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
          case (evaledLhs, evaledRhs) of
            (Int' l, Int' r) -> pure $ Bool' $ l > r
            (Int' r, x) -> typeErr stack 2 ">" "int" x
            (x, _) -> typeErr stack 1 ">" "int" x
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
        [arg] -> do
          evaledArg <- eval ctx arg
          case evaledArg of
            String' mod -> do
              let filename = currDir <> "/" <> mod <> ".hisp"
              program <- readFile filename
              case parse filename program of
                Left e -> errNoStack $ show e
                Right exprs -> do
                  let newCurrDir = takeDirectory filename
                  traverse_ (eval (Context globals M.empty newCurrDir [])) exprs
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
        _ -> arityErr stack "type"

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
  (List values1 _, List values2 _) -> values1 == values2
  (Symbol sym1 _, Symbol sym2 _) -> sym1 == sym2
  x -> v1 == v2

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

evalFn :: [StackEntry] -> M.Map String Value -> [Value] -> IO Value
evalFn stack captures values =
  case values of
    [List args _, body] -> do
      maybeVarArg <- case last2 args of
          Just (Symbol "&" _, lastArg) ->
            case lastArg of
              Symbol name _ -> pure $ Just name
              x -> err stack $ "Variadic function argument not a symbol, was: [" <> printVal x  <> "]"  
          _ -> pure Nothing
      let nonVarArgs = if isJust maybeVarArg then dropLast 2 args else args
      symArgs <- for nonVarArgs $ \arg ->
        case arg of
          Symbol name _ -> pure name
          x -> err stack $ "Function argument not a symbol, was: [" <> printVal x  <> "]"      
      pure $ Function symArgs maybeVarArg body captures False 
    [x, _] -> err stack $ "First argument to fn not a list, was: [" <> printVal x <> "]"
    _ -> err stack $ "Wrong number of arguments to fn"

quote :: Context -> Value -> IO Value
quote ctx@(Context globals locals _ stack) val =
  case val of
    List [Symbol "~" _, v] _ -> eval ctx v
    List vals pos -> do
      qVals <- traverse (quote ctx) vals
      pure $ List qVals pos
    Function args varArg body captures macro -> do
      qBody <- quote ctx body
      pure $ Function args varArg qBody captures macro
    v -> pure v