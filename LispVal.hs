module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--ignore first space
spaces :: Parser ()
spaces = skipMany1 space

--universal data type
data LispVal = Atom String
             | List [LispVal]  
             | DottedList [LispVal] LispVal
             | Number Integer
             | Bool Bool
             | String String

  
--create values of the specified types--
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many ( noneOf "\"" )
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
         <|> parseNumber
         <|> parseString

readExpr :: String -> String 
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err 
  Right val -> "Found value" 

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
  
