module Main where
import System.Environment	
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad


data LispVal = Atom String
			 | List [LispVal]
			 | DottedList [LispVal]
			 | Number Integer
			 | String String
			 | Bool Bool

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> symbol <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \num -> (return . Number . read) num

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber


spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
     Left err -> "No match: " ++ show err
     Right _ -> "Found value"
