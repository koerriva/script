module Main where

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL
import System.IO
import Control.Concurrent
import ServiceEngine (load)
import ScriptEngine (runFun)
import Type
import Script.Type (LispError(..))

main = do
    putStrLn "Thrift-like srcipt engine v0.1"
    service <- load "/home/yangyy/work/haskell/srcipt/script/hello.thrift"
    print service
    serverWith config (handler service)

config = Config {
    srvLog = stdLogger
  , srvHost = "127.0.0.1"
  , srvPort = 8080
}

handler :: TService -> Handler String
handler service sockAddr url request = do
    print url
    case lookService url service of
        (code@(2,0,0),Just f) -> do
            r <- execService url f
            case r of
                TException err  -> return $ response (5,0,1) "FAIL" err
                mapM_           -> return $ response code "Ok" (show r)
        (code@(4,0,4),_) -> return $ response code "Service Not Found" "Service Not Found"
        (code@(4,0,0),_) -> return $ response code "Bad Request" "Service Params Not Match"

lookService :: URL -> TService -> (ResponseCode,Maybe TFunction)
lookService url@(URL url_type url_path url_params) service@(TService sName sFunctions) =
    if url_path `elem` map ((\s -> sName++"/"++s) . funName) sFunctions then
        case lookScript url service of
            Just f -> ((2,0,0),Just f)
            Nothing -> ((4,0,0),Nothing)
        else ((4,0,4),Nothing)
    where
        lookScript :: URL -> TService -> Maybe TFunction
        lookScript url@(URL url_type url_path url_params) service@(TService sName sFunctions) =
            case filter (\(TFunction _ name params) -> sName++"/"++name == url_path) sFunctions of
                [] -> Nothing
                [f] -> if map fst url_params == map pName (funParams f) then Just f else Nothing

execService :: URL -> TFunction -> IO TType
execService (URL _ _ url_params) (TFunction _ fName _) = do
    let script =  "/home/yangyy/work/haskell/srcipt/script/hello.clj"
    result <- runFun script fName (map snd url_params )
    print result
    case result of
        Default val -> return $ TString (show val)
        _           -> return $ TException "service not impeletion"

response :: ResponseCode -> ResponseMsg -> ResponseData -> Response ResponseData
response code msg dat = Response code msg [json,size dat] dat
    where
        json = Header HdrContentType "application/json"
        size = Header HdrContentLength . show . length