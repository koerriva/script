{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL
import System.IO
import Control.Concurrent
import Control.Monad.Except
import ServiceEngine
import ScriptEngine (runScript)
import Type
import Script.Type (LispError(..),LispVal)
import Thrift.Type
import Data.Text (pack,unpack)

main = do
    putStrLn "Thrift-like srcipt engine v0.1"
    serverWith config handler

config = Config {
    srvLog = stdLogger
  , srvHost = "127.0.0.1"
  , srvPort = 8080
}

handler :: Handler String
handler sockAddr url request = do
    idl <- loadIDL "script/hello.thrift"
    print url
    if length (split (url_path url)) /= 2 then
        return $ response (4,0,4) "Service Not Found" "Service Not Found"
    else
        let sname = (split (url_path url))!!0
            fname = (split (url_path url))!!1
            params = url_params url in
        case loadService (sname,fname,params) idl of
            (code@(2,0,0),Just s) -> do
                r <- runService params s
                case r of
                    Nothing  -> return $ response (5,0,1) "FAIL" "Service Not Ready"
                    Just a   -> return $ response code "Ok" (show a)
            (code@(4,0,4),_) -> return $ response code "Service Not Found" "Service Not Found"
            (code@(4,0,0),_) -> return $ response code "Bad Request" "Service Params Not Match"
        where
            split = words . map (\c -> if c=='/' then ' ' else c)

            loadService :: (ServiceName,FunctionName,[Param]) -> IDL -> (ResponseCode,Maybe TFunction)
            loadService (sname,fname,params) idl =
                case findService sname idl of
                    Nothing -> ((4,0,4),Nothing)
                    Just s  -> case findFunction fname s of
                                    Nothing -> ((4,0,4),Nothing)
                                    Just f  -> (checkParams params f,Just f)
                where
                    checkParams :: [Param] -> TFunction -> ResponseCode
                    checkParams params f = if fParams f == map (pack . fst) params then (2,0,0) else (4,0,0)

            runService :: [Param] -> TFunction -> IO (Maybe LispVal)
            runService params f = do
                let script =  "script/hello.clj"
                    fname = (unpack . fName) f
                result <- runScript script fname (map snd params)
                print result
                case result of
                    Default val -> return $ Just val
                    _           -> return $ Nothing

            response :: ResponseCode -> ResponseMsg -> ResponseData -> Response ResponseData
            response code msg dat = Response code msg [json,size dat] dat
                where
                    json = Header HdrContentType "application/json"
                    size = Header HdrContentLength . show . length