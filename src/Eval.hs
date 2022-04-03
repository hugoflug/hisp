module Eval where

import Control.Monad (when, join)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Map ((!?), union)
import Data.Maybe (fromMaybe)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Lang
import Parse (parse)
import System.FilePath (takeDirectory)
import Text.Parsec (SourcePos)

-- Closures?
-- Line numbers at errs
-- Be more liberal with accepted symbols
-- CLI
-- Better REPL (repline?)
-- Improve exceptions
-- Automatically load core.hisp
-- Think about macroexpand
-- Could fn be a macro that just evals args?

data Context = Context {
  globals :: IORef (M.Map String Value),
  locals :: M.Map String Value,
  currentDir :: String
}

eval :: Context -> Value -> IO Value
eval ctx@(Context globals locals _) val =
  case val of
    Symbol name pos -> do
      case locals !? name of
        Just v -> pure v
        Nothing -> do
          reg <- readIORef globals
          case reg !? name of
            Just v -> pure v
            Nothing -> case parseBuiltin name of
              Just builtin -> pure $ Builtin' builtin
              Nothing -> errPos pos $ "No such symbol: " <> name

    List (head : tail) pos -> do
      evaledHead <- eval ctx head
      case evaledHead of
        Builtin' name -> 
          evalBuiltin ctx pos name tail
        Function args body macro -> do
          when (length args /= length tail) $
            errPos pos $ "Wrong number of arguments"
          fnTail <- if macro then
              pure tail
            else
              traverse (eval ctx) tail
          let fnLocals = M.fromList $ zip args fnTail
          eval ctx{locals = fnLocals} body
        x -> errPos pos $ "No function at head of list, was: [" <> printVal x <> "]"

    v -> pure v

fmtSourcePos :: SourcePos -> String
fmtSourcePos = show

printVal :: Value -> String 
printVal v = case v of
  Int' i -> show i
  String' i -> show i
  Bool' True -> "true"
  Bool' False -> "false"
  Symbol name _ -> name
  List vals _ -> "(" <> (printVals " " vals) <> ")"
  Function args val macro -> "function" -- printVal $ List [Symbol (if macro then "macro" else "fn"), List (Symbol <$> args), val]
  Nil -> "nil"

printVals :: String -> [Value] -> String
printVals separator = intercalate separator . fmap printVal

err :: String -> IO a
err = ioError . userError

errPos :: SourcePos -> String -> IO a
errPos pos msg = err $ msg <> " at " <> fmtSourcePos pos 

parseBuiltin :: String -> Maybe Builtin
parseBuiltin name =
  case name of
    "print" -> Just Print
    "+" -> Just Plus
    "def" -> Just Def
    "fn" -> Just Fn
    "macro" -> Just Macro
    "'" -> Just Quote
    "eval" -> Just Eval
    "cons" -> Just Cons
    "head" -> Just Head
    "tail" -> Just Tail
    "if" -> Just If
    "=" -> Just Equals
    "join" -> Just Join
    "split" -> Just Split
    "import" -> Just Import
    _ -> Nothing

evalBuiltin :: Context -> SourcePos -> Builtin -> [Value] -> IO Value
evalBuiltin ctx@(Context globals locals currDir) pos builtin values =
  case builtin of
    Print -> do
      evaledValues <- traverse (eval ctx) values
      putStrLn $ printVals " " evaledValues
      pure Nil
    Plus -> do
      evaledValues <- traverse (eval ctx) values
      intArgs <- for (zip [1..] evaledValues) $ \(ix, arg) ->
            case arg of
              Int' i -> pure i
              _ -> typeErr pos ix "+" "integer"
      pure $ Int' $ sum intArgs
    Def ->
      case values of
        [Symbol name _, val] -> do
          evaledVal <- eval ctx val
          modifyIORef globals $ \glob ->
            M.insert name evaledVal glob
          pure Nil
        [_, _] -> typeErr pos 1 "def" "symbol"
        _ -> arityErr pos "def"
    Fn -> evalFn pos False values
    Macro -> evalFn pos True values
    Quote ->
      case values of
        [value] -> quote ctx value
        _ -> arityErr pos "'"
    Eval ->
      case values of
        [value] -> do
          evaledValue <- eval ctx value
          eval ctx evaledValue
        _ -> arityErr pos "eval"
    Cons ->
      case values of
        [val, listArg] -> do
          evaledVal <- eval ctx val
          evaledList <- eval ctx listArg
          case evaledList of
            List list _ -> pure $ List (evaledVal : list) pos -- TODO: correct pos?
            _ -> typeErr pos 2 "cons" "list"
        _ -> arityErr pos "cons"
    Head ->
      case values of
        [val] -> do
          evaledList <- eval ctx val
          case evaledList of
            List (head : _) _ -> pure head
            _ -> typeErr pos 1 "head" "list"
        _ -> arityErr pos "head"
    Tail ->
      case values of
        [val] -> do
          evaledList <- eval ctx val
          case evaledList of
            List (_ : tail) _ -> pure $ List tail pos -- TODO: correct pos?
            _ -> typeErr pos 1 "tail" "list"
        _ -> arityErr pos "tail"
    If ->
      case values of
        [condition, then', else'] -> do
          evaledCondition <- eval ctx condition
          case evaledCondition of
            Bool' False -> eval ctx else'
            Nil -> eval ctx else'
            _ -> eval ctx then'
        _ -> arityErr pos "if"
    Equals ->
      case values of
        [lhs, rhs] -> do
          evaledLhs <- eval ctx lhs
          evaledRhs <- eval ctx rhs
          pure $ Bool' $ evaledLhs == evaledRhs
        _ -> arityErr pos "="
    Join ->
      case values of
        [arg] -> do
          evaledArg <- eval ctx arg
          list <- case evaledArg of
            List l _ -> pure l
            _ -> typeErr pos 1 "join" "list"
          stringArgs <- for list $ \arg ->
            case arg of
              String' s -> pure s
              v -> err $ "List in argument to join contained: " <> printVal v <> ". Only strings are allowed"
          pure $ String' $ join stringArgs
        _ -> arityErr pos "join"
    Split ->
      case values of
        [arg] -> do
          evaledArg <- eval ctx arg
          case arg of
            String' s -> pure $ List ((\c -> String' [c]) <$> s) pos
            _ -> typeErr pos 1 "split" "string"
        _ -> arityErr pos "split"
    Import ->
      case values of
        [arg] -> do
          case arg of
            String' mod -> do
              let filename = currDir <> "/" <> mod <> ".hisp"
              program <- readFile filename
              case parse filename program of
                Left e -> err $ show e
                Right exprs -> do
                  let newCurrDir = takeDirectory filename
                  traverse_ (eval (Context globals M.empty newCurrDir)) exprs
                  pure Nil
            _ -> typeErr pos 1 "import" "string"
        _ -> arityErr pos "import"


typeErr :: SourcePos -> Int -> String -> String -> IO a
typeErr pos argNo fnName typeName = errPos pos $ "Argument " <> show argNo <> " to " <> fnName <> " not of type " <> typeName

arityErr :: SourcePos -> String -> IO a
arityErr pos fnName = errPos pos $ "Wrong number of arguments to " <> fnName

evalFn :: SourcePos -> Bool -> [Value] -> IO Value
evalFn pos macro values =
  case values of
    [List args _, body] -> do
      symArgs <- for args $ \arg ->
        case arg of
          Symbol name _ -> pure name
          x -> errPos pos $ "Function argument not a symbol, was: [" <> printVal x  <> "]"
      pure $ Function symArgs body macro
    [x, _] -> errPos pos $ "First argument to fn not a list, was: [" <> printVal x <> "]"
    _ -> errPos pos $ "Wrong number of arguments to fn"

-- TODO: write in hisp instead?
quote :: Context -> Value -> IO Value
quote ctx@(Context globals locals _) val =
  case val of
    List [Symbol "~" _, v] _ -> eval ctx v
    List vals pos -> do
      qVals <- traverse (quote ctx) vals
      pure $ List qVals pos
    Function args body macro -> do
      qBody <- quote ctx body
      pure $ Function args qBody macro
    v -> pure v