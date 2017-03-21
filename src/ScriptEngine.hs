module ScriptEngine where


import Control.Monad
import Control.Monad.Except
import System.IO
import Script.Type
import Script.Core
import Script.Exception

find = undefined

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Clojure>>> ") . evalAndPrint

runFun :: String -> String -> [String] -> IO LispError
runFun file func params = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String params)]
    runSafeIOThrows $ do
        liftM Default $ eval env (List [Atom "load", String file])
        liftM Default $ eval env (List [Atom "apply", Atom func, Atom "args"])

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr