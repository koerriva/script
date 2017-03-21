{-# LANGUAGE ExistentialQuantification #-}
module Type where

import Control.Monad.Except
import Text.Parsec.Error
import Data.IORef
import Script.Type

type Service = IO
type TToken = String
data TType = TString String
           | TInt32 Integer
           | TBool Bool
           | TStruct Struct
           | TList [TType]
           | TVoid
           | TException String
           deriving(Eq)

instance Show TType where
    show (TString a) = a
    show (TInt32 a) = show a
    show (TBool a) = show a
    show (TStruct a) = show a
    show (TList as) = show $ map show as
    show TVoid      = "()"

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

s2t :: LispVal -> TType
s2t (String val)  = TString val
s2t (Number val)  = TInt32 val
s2t (Bool val)    = TBool val
s2t (List vals)   = TList (map s2t vals)
s2t (Vector vals) = s2t (List vals)