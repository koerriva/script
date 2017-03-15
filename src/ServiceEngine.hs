module ServiceEngine where

import Type
import Text.Parsec
import Text.ParserCombinators.Parsec

load :: String -> Service TService
load filename = do
    dat <- parseFromFile service filename
    case dat of
        Left err -> error $ show err
        Right a -> return a


stringRef :: Parser TTypeRef
stringRef = do{string "string";return TStringRef}
intRef :: Parser TTypeRef
intRef = do{string "i32";return TIntRef}
boolRef :: Parser TTypeRef
boolRef = do{string "bool";return TBoolRef}
structRef :: Parser TTypeRef
structRef = do{upper;many1 letter;return TStructRef}
listRef :: Parser TTypeRef
listRef = do{string "list";a <- between (char '<') (char '>') typeRef;return $ TListRef a}


typeRef :: Parser TTypeRef
typeRef = choice [stringRef,intRef,boolRef,structRef,listRef]

parameter :: Parser Param
parameter = do
    pos <- many1 digit
    char ':'
    valRef <- typeRef
    many1 space
    valName <- many1 letter
    return Param {
        pIndex = read pos :: Int,
        pName = valName,
        pType = valRef,
        pDefaultValue = Nothing
    }

parameters :: Parser [Param]
parameters = sepBy parameter (char ',')

function :: Parser TFunction
function = do
    skipMany space
    fRetRef <- optionMaybe typeRef
    many1 space
    fName <- many1 letter
    params <- between (char '(') (char ')') parameters
    newline
    return TFunction {
        funReturnType = fRetRef
       ,funName = fName
       ,funParams = params
    }

service :: Parser TService
service = do
    string "service"
    many1 space
    sName <- many1 letter
    many1 space
    char '{'
    newline
    functions <- many function
    char '}'
    return TService {serviceName=sName,serviceFunctions=functions}