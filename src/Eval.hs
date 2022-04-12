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

-- Execute multiple side-effectful functions after each other
-- nand built-in
-- Refer to previous lets inside a let binding
-- Be more liberal with accepted symbols
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

eval :: Context -> Value -> IO Value
eval ctx@(Context globals locals _ stack) val =
  case val of
    Symbol name pos -> do
      let newStack = (StackEntry name pos):stack
      case locals !? name of
        Just v -> pure v
        Nothing -> do
          reg <- readIORef globals
          case reg !? name of
            Just v -> pure v
            Nothing -> case parseBuiltin name of
              Just builtin -> pure $ Builtin' builtin
              Nothing -> do
                errPos newStack $ "No such symbol [" <> name <> "]"

    List (head : tail) pos -> do
      let 
        fnName = 
          case head of 
            (Symbol name _) -> name
            _ -> "(anon)"
      let newStack = (StackEntry fnName pos):stack
      evaledHead <- eval ctx head
      case evaledHead of
        Builtin' name -> 
          evalBuiltin ctx{stack = newStack} name tail
        Function args varArg body captures isMacro -> do
          when (length tail < length args) $ errPos newStack "Too few arguments"
          when (length tail > length args && isNothing varArg) $ errPos newStack "Too many arguments"

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

        x -> errPos newStack $ "No function at head of list, was: [" <> printVal x <> "]"

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
  Function args varArg val _ macro -> "function" -- printVal $ List [Symbol (if macro then "macro" else "fn"), List (Symbol <$> args), val]
  Builtin' builtin -> show builtin
  Nil -> "nil"

printVals :: String -> [Value] -> String
printVals separator = intercalate separator . fmap printVal

err :: String -> IO a
err = error

errPos :: [StackEntry] -> String -> IO a
errPos stack msg = err $ "Error: " <> msg <> " at: \n" <> fmtStack stack 

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
    "macro" -> Just Macro
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
    _ -> Nothing

evalBuiltin :: Context -> Builtin -> [Value] -> IO Value
evalBuiltin ctx@(Context globals locals currDir stack) builtin values =
  case builtin of
    Print -> do
      evaledValues <- traverse (eval ctx) values
      putStrLn $ printVals " " evaledValues
      pure Nil
    Readfile -> do
      case values of
        [arg] -> do
          evaledArg <- eval ctx arg
          case evaledArg of
            String' filename -> do
              contents <- readFile filename
              pure $ String' contents
            x -> typeErr stack 1 "read-file" "string" x
        _ -> arityErr stack "read-file"
    -- TODO: properly handle no args for arithmetic ops
    Plus -> do
      evaledValues <- traverse (eval ctx) values
      intArgs <- for (zip [1..] evaledValues) $ \(ix, arg) ->
            case arg of
              Int' i -> pure i
              x -> typeErr stack ix "+" "integer" x
      pure $ Int' $ sum intArgs
    Mult -> do
      evaledValues <- traverse (eval ctx) values
      intArgs <- for (zip [1..] evaledValues) $ \(ix, arg) ->
            case arg of
              Int' i -> pure i
              x -> typeErr stack ix "+" "integer" x
      pure $ Int' $ product intArgs
    Minus -> do
      evaledValues <- traverse (eval ctx) values
      intArgs <- for (zip [1..] evaledValues) $ \(ix, arg) ->
            case arg of
              Int' i -> pure i
              x -> typeErr stack ix "-" "integer" x
      pure $ Int' $ foldl' (-) (head intArgs) (tail intArgs)
    Nand -> do
      evaledValues <- traverse (eval ctx) values
      boolArgs <- for (zip [1..] evaledValues) $ \(ix, arg) ->
            case arg of
              Bool' i -> pure i
              x -> typeErr stack ix "-" "bool" x
      pure $ Bool' $ not $ and boolArgs
    Def ->
      case values of
        [Symbol name _, val] -> do
          evaledVal <- eval ctx val
          modifyIORef globals $ \glob ->
            M.insert name evaledVal glob
          pure Nil
        [x, _] -> typeErr stack 1 "def" "symbol" x
        _ -> arityErr stack "def"
    Fn -> evalFn stack False locals values
    Macro -> evalFn stack True locals values
    Quote ->
      case values of
        [value] -> quote ctx value
        _ -> arityErr stack "'"
    Eval ->
      case values of
        [value] -> do
          evaledValue <- eval ctx value
          eval ctx evaledValue
        _ -> arityErr stack "eval"
    Apply ->
      case values of
        [fn, list] -> do
          evaledList <- eval ctx list
          evaledList' <- case evaledList of
            (List ls _) -> pure ls
            x -> typeErr stack 2 "apply" "list" x
          eval ctx (List (fn:evaledList') (pos (head stack)))
        _ -> arityErr stack "apply"
    Cons ->
      case values of
        [val, listArg] -> do
          evaledVal <- eval ctx val
          evaledList <- eval ctx listArg
          case evaledList of
            List list _ -> pure $ List (evaledVal : list) (pos (head stack))
            x -> typeErr stack 2 "cons" "list" x
        _ -> arityErr stack "cons"
    Head ->
      case values of
        [val] -> do
          evaledList <- eval ctx val
          case evaledList of
            List (head : _) _ -> pure head
            List [] _ -> errPos stack "Head called on empty list"
            x -> typeErr stack 1 "head" "list" x
        _ -> arityErr stack "head"
    Tail ->
      case values of
        [val] -> do
          evaledList <- eval ctx val
          case evaledList of
            List (_ : tail) _ -> pure $ List tail (pos (head stack))
            x -> typeErr stack 1 "tail" "list" x
        _ -> arityErr stack "tail"
    If ->
      case values of
        [condition, then', else'] -> do
          evaledCondition <- eval ctx condition
          case evaledCondition of
            Bool' False -> eval ctx else'
            Nil -> eval ctx else'
            _ -> eval ctx then'
        _ -> arityErr stack "if"
    Equals ->
      case values of
        [lhs, rhs] -> do
          evaledLhs <- eval ctx lhs
          evaledRhs <- eval ctx rhs
          pure $ Bool' $ evaledLhs `eq` evaledRhs
        _ -> arityErr stack "="
    Gt ->
      case values of
        [lhs, rhs] -> do
          evaledLhs <- eval ctx lhs
          evaledRhs <- eval ctx rhs
          case (evaledLhs, evaledRhs) of
            (Int' l, Int' r) -> pure $ Bool' $ l > r
            (Int' r, x) -> typeErr stack 2 ">" "int" x
            (x, _) -> typeErr stack 1 ">" "int" x
        _ -> arityErr stack ">"
    Str -> do
      evaledValues <- traverse (eval ctx) values
      let 
        toString val = 
          case val of
            String' s -> s
            x -> printVal x
      pure $ String' $ intercalate "" $ toString <$> evaledValues
    Split ->
      case values of
        [arg] -> do
          evaledArg <- eval ctx arg
          case evaledArg of
            String' s -> pure $ List ((\c -> String' [c]) <$> s) (pos (head stack))
            x -> typeErr stack 1 "split" "string" x
        _ -> arityErr stack "split"
    Import ->
      case values of
        [arg] -> do
          evaledArg <- eval ctx arg
          case evaledArg of
            String' mod -> do
              let filename = currDir <> "/" <> mod <> ".hisp"
              program <- readFile filename
              case parse filename program of
                Left e -> err $ show e
                Right exprs -> do
                  let newCurrDir = takeDirectory filename
                  traverse_ (eval (Context globals M.empty newCurrDir [])) exprs
                  pure Nil
            x -> typeErr stack 1 "import" "string" x
        _ -> arityErr stack "import"
    Error ->
      case values of
        [arg] -> do
          evaledArg <- eval ctx arg
          case evaledArg of
            String' msg -> do
              errPos stack $ msg
            x -> typeErr stack 1 "error" "string" x
        _ -> arityErr stack "error"

eq :: Value -> Value -> Bool
eq v1 v2 = case (v1, v2) of
  (List values1 _, List values2 _) -> values1 == values2
  (Symbol sym1 _, Symbol sym2 _) -> sym1 == sym2
  x -> v1 == v2

typeErr :: [StackEntry] -> Int -> String -> String -> Value -> IO a
typeErr stack argNo fnName expected actual = errPos stack $ "Argument " <> show argNo <> " to " <> fnName <> " not of type " <> expected <> " (was " <> printVal actual <> ")"

arityErr :: [StackEntry] -> String -> IO a
arityErr stack fnName = errPos stack $ "Wrong number of arguments to " <> fnName

last2                    :: [a] -> Maybe (a, a)
last2 [x1, x2]           =  Just (x1, x2)
last2 (_:xs)             =  last2 xs
last2 []                 =  Nothing

dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

evalFn :: [StackEntry] -> Bool -> M.Map String Value -> [Value] -> IO Value
evalFn stack macro captures values =
  case values of
    [List args _, body] -> do
      maybeVarArg <- case last2 args of
          Just (Symbol "&" _, lastArg) ->
            case lastArg of
              Symbol name _ -> pure $ Just name
              x -> errPos stack $ "Variadic function argument not a symbol, was: [" <> printVal x  <> "]"  
          _ -> pure Nothing
      let nonVarArgs = if isJust maybeVarArg then dropLast 2 args else args
      symArgs <- for nonVarArgs $ \arg ->
        case arg of
          Symbol name _ -> pure name
          x -> errPos stack $ "Function argument not a symbol, was: [" <> printVal x  <> "]"      
      pure $ Function symArgs maybeVarArg body captures macro 
    [x, _] -> errPos stack $ "First argument to fn not a list, was: [" <> printVal x <> "]"
    _ -> errPos stack $ "Wrong number of arguments to fn"

-- TODO: write in hisp instead?
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