module Script.Parser where

import Script.Type
import Text.Parsec hiding(try)
import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Except

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x
parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr (try (do{many space;char ',';many space;return ()}) <|> spaces)
parseVector :: Parser LispVal
parseVector = liftM Vector $ sepBy parseExpr (try (do{many space;char ',';many space;return ()}) <|> spaces)
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x
         <|> do char '['
                x <- parseVector
                char ']'
                return x
parserList :: Parser Lisp
parserList = sepBy parseExpr newline

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "script" input of
     Left err -> throwError $ Parser err
     Right val -> return val

loadScript :: String -> Script Lisp
loadScript filename = do
    dat <- parseFromFile parserList filename
    case dat of
        Left err -> error $ show err
        Right val -> return val