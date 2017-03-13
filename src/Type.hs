module TType where

type Service = IO
type Script = IO
type TToken = String
data TType = TString String
           | TInt Int
           | TBool Bool
           | TStruct Struct
           | TList [TType]
           deriving(Show,Eq)

data TTypeRef = TStringRef
              | TIntRef
              | TBoolRef
              | TStructRef
              | TListRef TTypeRef
              deriving(Show,Eq)

data Param = Param {
    pIndex :: Int,
    pName :: String,
    pType :: TTypeRef,
    pDefaultValue :: Maybe TType
} deriving(Show,Eq)

data Struct = Struct {
    sName::String,
    sFields::[Param]
} deriving(Show,Eq)

data TFunction = TFunction {
    funReturnType::Maybe TTypeRef,
    funName::TToken,
    funParams::[Param]
} deriving(Show,Eq)

data TService = TService {
    serviceName :: TToken
   ,serviceFunctions :: [TFunction]
} deriving(Show,Eq)

type ResponseCode = (Int,Int,Int)
type ResponseMsg = String
type ResponseData = String

data JVal = JString String
          | JInt    Int
          | JBool   Bool
          | JObj    [(JToken,JVal)]
          | JArray  [JVal]
          | JVoid
          deriving (Show,Eq)
type JToken = String
data JOp = JStrOp String
         | JAddOp JField JField
         | JFunOp [JField]
         deriving(Show,Eq)

data JField = JFVal JVal
            | JFVar JVar
            deriving (Show,Eq)
data JVar = JVar (JToken,JVal)
          | JExp (JToken,JOp)
          deriving (Show,Eq)

data JStatement = JVarDef JVar
                | JReturn JField
                deriving(Show,Eq)

data JFunction = JFunction {
    name::JToken,
    params::[JToken],
    vars::[JVar],
    ret::Maybe JField
}deriving(Show,Eq)

data JScript = JScript {
    globalVar::[JVar],
    defs::[JFunction]
}deriving(Show,Eq)

j2t :: JVal -> TType
j2t (JString a) = TString a
j2t (JInt a) = TInt a
j2t (JBool a) = TBool a
j2t (JArray a) = TList (map j2t a)