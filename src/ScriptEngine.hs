module ScriptEngine where

import TType
import Text.Parsec hiding(try)
import Text.ParserCombinators.Parsec
import Data.String.Utils(replace)

loadScript = load

load :: String -> Script JScript
load filename = do
    dat <- parseFromFile script filename
    case dat of
        Left err -> error $ show err
        Right a -> return a

find :: JToken -> JScript -> Maybe JFunction
find fName (JScript vr fs) =
    case filter (\f -> name f == fName) fs of
        [] -> Nothing
        [f] -> Just f

eval = evalJFunction

script :: Parser JScript
script = do
    vs <- many var
    fs <- many def
    return $ JScript vs fs

jstr = do
    char '"'
    a <- many (letter<|>digit<|>noneOf "\""<|>space<|>oneOf "$")
    char '"'
    return $ JString a
jint = do{a <- many1 digit;return $ JInt (read a :: Int)}
jbool = do
    a <- do{string "true";return True} <|> do{string "false";return False}
    return $ JBool a
jobj = do
    char '{'
    a <- sepBy1 (do{name <- many1 letter;char ':';v <- val;return (name,v)}) (char ',')
    char '}'
    return $ JObj a
jarray = do{a <- many1 (jstr <|> jint <|> jbool <|> jobj);return $ JArray a}

val :: Parser JVal
val = choice [jstr,jint,jbool,jobj,jarray]

var :: Parser JVar
var = do
    string "var"
    many1 space
    vName <- many1 letter
    many space
    char '='
    many space
    val <- val
    return $ JVar (vName,val)

varName (JVar (vName,_)) = vName
varName (JExp (vName,_)) = vName

evalVars :: [JVar] -> [(JToken,JVal)] -> [(JToken,JVal)]
evalVars [] stack = stack
evalVars [v] stack = (varName v,evalVar v stack):stack
evalVars (v:ls) stack = evalVars ls (evalVars [v] stack)

evalVar :: JVar -> [(JToken,JVal)] -> JVal
evalVar (JVar (vName,vVal)) _ = vVal
evalVar (JExp (vName,op)) ls = evalOp op ls

evalOp :: JOp -> [(JToken,JVal)] -> JVal
evalOp (JStrOp exp) l = JString $ foldl (\s (name,JString val) -> replace ("$"++name) val s) exp l
evalOp (JAddOp (JFVal (JInt a)) (JFVal (JInt b))) l = JInt (a+b)
evalOp _ _ = undefined


fRet :: Parser JField
fRet = do
    many space
    string "return"
    do{a <- val;return $ JFVal a} <|> do{a <- var;return $ JFVar a}

statement :: Parser JStatement
statement = do
    many space
    v <- var


def :: Parser JFunction
def = do
    string "function"
    many1 space
    fName <- many1 letter
    char '('
    fParams <- sepBy (many1 letter) (char ',')
    char ')'
    char '{';newline
    many statement
    newline;char '}'
    return JFunction{
        name=fName,params=fParams,vars=[],ret=Nothing
    }

evalJFunction :: [JVal] -> JFunction -> JVal
evalJFunction vals (JFunction _ params vars ret) =
    let ps = zip params vals
        stack = evalVars vars ps
    in
        case ret of
            Nothing -> JVoid
            Just (JFVal val) -> val
            Just (JFVar var) -> evalVar var stack