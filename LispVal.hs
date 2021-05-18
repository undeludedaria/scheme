module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Debug.Trace


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--ignore first space
spaces :: Parser ()
spaces = skipMany space

--universal data type
data LispVal = Atom String
             | List [LispVal]  
             | DottedList [LispVal] LispVal
             | Number Integer
             | Bool Bool
             | String String
              deriving Show


escapeQuote :: Parser Char 
escapeQuote = char '\\'
           <|> digit
           <|> symbol
           <|> letter
  

--create values of the specified types--
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapeQuote) 
  char '"'
  return $ String x --Parser action that consumes no input but returns it as the inner value, thus the 'Parser LispVal' type

parseAtom :: Parser LispVal
parseAtom = do
  first <- symbol <|> letter
  rest <- many(symbol <|> letter <|> digit)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  x <- many1 digit
  return $ Number (read x)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString 
         <|> parseNumber

readExpr :: String -> String 
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
  Left err -> "No match: " ++ show err 
  Right val -> "Found value: " ++ show val 

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
  
