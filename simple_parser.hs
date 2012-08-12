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
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of 
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\t' 

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

parseBool :: Parser LispVal 
parseBool = oneOf string "#"
                  x <- oneOf "tf"
                  return $ case x of 
                    't' -> Bool True
                    'f' -> Fool False

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> symbol <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseDigital1 :: Parser LispVal
parseDigital1 = do x many1 digit
                   (return . Number . read) x

parseDigital2 :: Parser LispVal
parseDigital2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
            x <- many1 hexDigit
            return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs
                         

parseNumber :: Parser LispVal
parseNumber = do num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
                 return $ num

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseBool


spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
     Left err -> "No match: " ++ show err
     Right _ -> "Found value"
