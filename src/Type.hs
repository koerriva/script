{-# LANGUAGE ExistentialQuantification #-}
module Type where

import Control.Monad.Except
import Text.Parsec.Error
import Data.IORef

type Service = IO
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