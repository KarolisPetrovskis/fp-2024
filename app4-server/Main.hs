{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Chan
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict as S (StateT, evalStateT, get)
import Data.String.Conversions
import GHC.Conc (forkIO)
import Lib2 qualified
import Lib3 qualified
import Web.Scotty

type AppState = (TVar Lib2.State, Chan Lib3.StorageOp)

cmd :: String -> StateT AppState IO String
cmd str = do
  case Lib2.parse Lib3.parseCommand str of
    (Left e, _) -> liftIO $ return ("PARSE ERROR: " ++ e)
    (Right c, "") -> do
      (st, chan) <- S.get
      tr <- liftIO $ Lib3.stateTransition st c chan
      case tr of
        Left e2 -> return ("ERROR: " ++ e2)
        Right m -> do
          case m of
            Just msg -> return msg
            Nothing -> return ""
    (Right _, r) -> return ("PARSE ERROR: string is not fully consumed - " ++ r)

main :: IO ()
main = do
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  state <- newTVarIO Lib2.emptyState
  _ <- forkIO $ Lib3.storageOpLoop chan

  scotty 3000 $
    post "/" $ do
      b <- body
      liftIO $ putStrLn ("\nRequest was: " ++ cs b)

      result <- liftIO $ try (evalStateT (cmd (cs b)) (state, chan)) :: ActionM (Either SomeException String)

      case result of
        Left ex -> do
          liftIO $ putStrLn $ "Error: " ++ show ex
          text "Response: Error occurred while processing your request."
        Right responseMsg -> do
          text $ cs responseMsg
