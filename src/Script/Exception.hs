module Script.Exception where

import Script.Type
import Control.Monad.Except

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

trapSafeError :: MonadError a m => m a -> m a
trapSafeError action = catchError action return

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Right val) = return val
liftThrows (Left err) = throwError err

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

runSafeIOThrows :: IOThrowsError LispError -> IO LispError
runSafeIOThrows action = runExceptT (trapSafeError action) >>= return . extractValue