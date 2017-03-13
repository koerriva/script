module Main where

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL
import System.IO
import Control.Concurrent
import ServiceEngine (load)
import ScriptEngine (loadScript,find,eval)
import TType

main = do
    putStrLn "Thrift-like srcipt engine v0.1"
    service <- load "d:/work/script/script/hello.thrift"
    print service
    serverWith config (handler service)

config = Config {
    srvLog = stdLogger
  , srvHost = "127.0.0.1"
  , srvPort = 80
}

handler :: TService -> Handler String
handler service sockAddr url request = do
    print url
    case lookService url service of
        (code@(2,0,0),Just f) -> do
            r <- execFunction url f
            return $ response code "Ok" (show r)
        (code@(4,0,4),_) -> return $ response code "Service Not Found" "Service Not Found"
        (code@(4,0,0),_) -> return $ response code "Bad Request" "Service Params Not Match"

lookService :: URL -> TService -> (ResponseCode,Maybe TFunction)
lookService url@(URL url_type url_path url_params) service@(TService sName sFunctions) =
    if url_path `elem` map ((\s -> sName++"/"++s) . funName) sFunctions then
        case lookFunction url service of
            Just f -> ((2,0,0),Just f)
            Nothing -> ((4,0,0),Nothing)
        else ((4,0,4),Nothing)

lookFunction :: URL -> TService -> Maybe TFunction
lookFunction url@(URL url_type url_path url_params) service@(TService sName sFunctions) =
    case filter (\(TFunction _ name params) -> sName++"/"++name == url_path) sFunctions of
        [] -> Nothing
        [f] -> if map fst url_params == map pName (funParams f) then Just f else Nothing

execFunction :: URL -> TFunction -> IO TType
execFunction (URL _ _ url_params) (TFunction _ fName _) = do
    script <- loadScript "d:/work/script/script/hello.script"
    let params = map (JString . snd) url_params
    case find fName script of
        Nothing -> return $ TString "service not impeletion"
        Just jf -> return $ j2t (eval params jf)

response :: ResponseCode -> ResponseMsg -> ResponseData -> Response ResponseData
response code msg dat = Response code msg [json,size dat] dat
    where
        json = Header HdrContentType "application/json"
        size = Header HdrContentLength . show . length