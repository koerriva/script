module Type where

type Service = IO

data Thrift = Key String
            | String String
            | Fun String