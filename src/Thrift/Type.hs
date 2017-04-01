module Thrift.Type where

import Language.Thrift.AST
import Text.Megaparsec.Pos
import Data.Text hiding (map)

type ServiceName = String
type FunctionName = String
type IDL = Program SourcePos
type TFunction = Function SourcePos
data TVal = TInt Integer
          | TFloat Double
          | TLiteral Text
          | TIdentifier Text
          | TList [TVal]
          | TMap [(TVal,TVal)]
          deriving (Show,Eq)