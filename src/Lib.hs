module Lib
  ( eval
  , trapErr
  , extractValue
  , readExpr
  ) where

import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal]
               LispVal
  | Number Integer
  | String String
  | Bool Bool

instance Show LispVal where
  show = showVal

isLispNumber :: LispVal -> Bool
isLispNumber (Number _) = True
isLispNumber _ = False

isLispAtom :: LispVal -> Bool
isLispAtom (Atom _) = True
isLispAtom _ = False

isLispString :: LispVal -> Bool
isLispString (String _) = True
isLispString _ = False

data LispErr
  = NumArgs Integer
            [LispVal]
  | TypeMismatch String
                 LispVal
  | Parser ParseError
  | BadSpecialFrom String
                   LispVal
  | NotFunction String
                String
  | UnboundVar String
               String
  | Default String

instance Error LispErr where
  noMsg = Default "An error has occured"
  strMsg = Default

instance Show LispErr where
  show = showErr

type ThrowsError = Either LispErr

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = [first] ++ rest
  return $
    case atom of
      "#t" -> Bool True
      "#f" -> Bool False
      _ -> Atom atom

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseSExpr :: Parser LispVal
parseSExpr = do
  _ <- char '('
  x <- (try parseList) <|> parseDottedList
  _ <- char ')'
  return x

parseExpr :: Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> parseSExpr

showVal :: LispVal -> String
showVal (String str) = "\"" ++ str ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List ls) = "(" ++ unwordsList ls ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

showErr :: LispErr -> String
showErr (UnboundVar message varname) = message ++ ": " ++ varname
showErr (BadSpecialFrom message form) = message ++ ": " ++ show form
showErr (NotFunction message func) = message ++ ": " ++ show func
showErr (NumArgs expected []) =
  "Expected " ++ show expected ++ " args; no values found"
showErr (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ values
  where
    values =
      if (length found) <= 5
        then unwordsList found
        else (take 5 . unwordsList) found ++ " ... "
showErr (TypeMismatch expected found) =
  "Invalid type: expected \"" ++ expected ++ "\", found " ++ show found
showErr (Parser parseErr) = "Parser error at " ++ show parseErr

trapErr :: Either LispErr String -> Either LispErr String
trapErr action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop 0 (+))
  , ("-", numericBinop 0 (-))
  , ("*", numericBinop 1 (*))
  , ("/", numericBinop 1 div)
  , ("mod", numericBinop 1 mod)
  , ("quotient", numericBinop 1 quot)
  , ("remainder", numericBinop 1 rem)
  , ("symbol?", isBinop isLispAtom)
  , ("string?", isBinop isLispString)
  , ("number?", isBinop isLispNumber)
  ]

numericBinop ::
     Integer
  -> (Integer -> Integer -> Integer)
  -> [LispVal]
  -> ThrowsError LispVal
numericBinop id op [] = return $ Number id
numericBinop id op singleVal@[x] = do
  num <- unpackNum x
  return $ Number (op id num)
numericBinop _ op params = mapM unpackNum params >>= return . Number . foldl1 op

isBinop :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
isBinop check val@[x] = return $ Bool $ check x
isBinop _ xs = throwError $ NumArgs 1 xs

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func:args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialFrom "Unrecongnized special form" badForm
