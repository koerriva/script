module ServiceEngine where

import Language.Thrift.AST
import Language.Thrift.Parser
import Text.Megaparsec.Pos
import qualified Data.Text as T
import Thrift.Type

loadIDL :: FilePath -> IO IDL
loadIDL filePath = do
    dat <- parseFromFile filePath
    case dat of
        Left err -> (error . show) err
        Right a -> return a

findService :: ServiceName -> IDL -> Maybe (Service SourcePos)
findService sname idl =  find (\name (ServiceDefinition s) -> if serviceName s == T.pack name then Just s else Nothing) sname (programDefinitions idl)

findFunction :: FunctionName -> Service SourcePos -> Maybe (Function SourcePos)
findFunction fname s = find (\name f -> if functionName f == T.pack name then Just f else Nothing) fname (serviceFunctions s)
fName = functionName
fParams = (map fieldName) . functionParameters

find :: (Eq c) => (a -> b -> Maybe c) ->  a -> [b] -> Maybe c
find f a [] = Nothing
find f a [b] = f a b
find f a (b:ls) = let x = find f a [b] in if x == Nothing then find f a ls else x

fromConstVal :: ConstValue src -> TVal
fromConstVal (ConstInt a _)  = TInt a
fromConstVal (ConstFloat a _) = TFloat a
fromConstVal (ConstLiteral a _) = TLiteral a
fromConstVal (ConstList ls _) = TList $ map fromConstVal ls
fromConstVal (ConstMap ls _) = TMap $ map (\(k,v) -> (fromConstVal k,fromConstVal v)) ls