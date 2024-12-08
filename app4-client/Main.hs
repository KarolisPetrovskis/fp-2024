{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Lens
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString
import Data.IORef
import Data.String.Conversions
import qualified Lib2
import Control.Monad (when)
import Network.Wreq
import System.IO as SIO
import qualified Lib3

data MyDomainAlgebra next
  = Load (() -> next)
  | AddComponent String (() -> next)
  | RemoveComponent Int (() -> next)
  | SetNextId Int (() -> next)
  | ShowFacility (String -> next)
  | CalculateTotalProduction (String -> next)
  | CalculateTotalStorage (String -> next)
  | Save (() -> next)
  deriving (Functor)

type MyDomain = Free MyDomainAlgebra

load :: MyDomain ()
load = liftF $ Load id

addComponent :: String -> MyDomain ()
addComponent component = liftF $ AddComponent component id

removeComponent :: Int -> MyDomain ()
removeComponent n = liftF $ RemoveComponent n id

setNextId :: Int -> MyDomain ()
setNextId n = liftF $ SetNextId n id

showFacility :: MyDomain String
showFacility = liftF $ ShowFacility id

calculateTotalProduction :: MyDomain String
calculateTotalProduction = liftF $ CalculateTotalProduction id

calculateTotalStorage :: MyDomain String
calculateTotalStorage = liftF $ CalculateTotalStorage id

save :: MyDomain ()
save = liftF $ Save id

program :: MyDomain String
program = do
  load
  showFacility

-- putStrLn $ cs $ resp ^. responseBody
runHttp :: MyDomain a -> IO a
runHttp (Pure a) = return a
runHttp (Free step) = do
  next <- runStep step
  runHttp next
  where
    runStep :: MyDomainAlgebra a -> IO a
    runStep (Load next) = do
      let rawRequest = cs "load" :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next ()
    runStep (AddComponent component next) = do
      let rawRequest = cs ("add(" ++ component ++ ")") :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next ()
    runStep (RemoveComponent n next) = do
      let rawRequest = cs ("remove(" ++ show n ++ ")") :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next ()
    runStep (SetNextId n next) = do
      let rawRequest = cs ("set_next_id(" ++ show n ++ ")") :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next ()
    runStep (ShowFacility next) = do
      let rawRequest = cs "show_facility" :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next (cs $ resp ^. responseBody)
    runStep (CalculateTotalProduction next) = do
      let rawRequest = cs "calculate_total_production" :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next (cs $ resp ^. responseBody)
    runStep (CalculateTotalStorage next) = do
      let rawRequest = cs "calculate_total_storage" :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next (cs $ resp ^. responseBody)
    runStep (Save next) = do
      let rawRequest = cs "save" :: ByteString
      resp <- post "http://localhost:3000" rawRequest
      return $ next ()


type SmartExecutorState = IORef (Bool, Bool, Bool, [String])
runSmartExecutor :: SmartExecutorState -> MyDomain a -> IO a
runSmartExecutor _ (Pure a) = return a
runSmartExecutor stateRef (Free step) = do
    next <- processStep stateRef step
    runSmartExecutor stateRef next
  where
    processStep :: SmartExecutorState -> MyDomainAlgebra a -> IO a
    processStep stateRef' (Load next) = do
        writeIORef stateRef' (True, True, False, [])
        return $ next ()

    processStep stateRef' (Save next) = do
      (_, load', changed, commands) <- readIORef stateRef
      if changed
          then do
              when load' $ do
                let rawRequest = cs "load" :: ByteString
                resp1 <- post "http://localhost:3000" rawRequest
                return ()

              when (commands /= []) $ do
                let rawRequest = cs (unlines commands) :: ByteString
                resp2 <- post "http://localhost:3000" rawRequest
                return ()

              let rawRequest = cs "save" :: ByteString
              resp3 <- post "http://localhost:3000" rawRequest

              writeIORef stateRef' (load', False, False, [])
          else
              putStrLn "No commands to save."
      return $ next ()

    processStep stateRef' (AddComponent component next) = do
        modifyQueue stateRef' $ "add(" ++ component ++ ")"
        return $ next ()

    processStep stateRef' (RemoveComponent n next) = do
        modifyQueue stateRef' $ "remove(" ++ show n ++ ")"
        return $ next ()

    processStep stateRef' (SetNextId n next) = do
        modifyQueue stateRef' $ "set_next_id(" ++ show n ++ ")"
        return $ next ()

    processStep stateRef' (ShowFacility next) = do
        (loaded, load', changed, commands) <- readIORef stateRef'

        when load' $ do
            let rawRequest = cs "load" :: ByteString
            resp1 <- post "http://localhost:3000" rawRequest
            return ()

        when (commands /= []) $ do
                let rawRequest = cs (unlines commands) :: ByteString
                resp2 <- post "http://localhost:3000" rawRequest
                return ()
        writeIORef stateRef' (loaded, False, changed, [])

        let rawRequest = cs "show_facility" :: ByteString
        resp3 <- post "http://localhost:3000" rawRequest

        return $ next (cs $ resp3 ^. responseBody)

    processStep stateRef' (CalculateTotalProduction next) = do
        (loaded, load, changed, commands) <- readIORef stateRef'

        when load $ do
            let rawRequest = cs "load" :: ByteString
            resp1 <- post "http://localhost:3000" rawRequest
            return ()

        when (commands /= []) $ do
                let rawRequest = cs (unlines commands) :: ByteString
                resp2 <- post "http://localhost:3000" rawRequest
                return ()
        writeIORef stateRef' (loaded, False, changed, [])

        let rawRequest = cs "calculate_total_production" :: ByteString
        resp3 <- post "http://localhost:3000" rawRequest

        return $ next (cs $ resp3 ^. responseBody)

    processStep stateRef' (CalculateTotalStorage next) = do
        (loaded, load', changed, commands) <- readIORef stateRef'

        when load' $ do
            let rawRequest = cs "load" :: ByteString
            resp1 <- post "http://localhost:3000" rawRequest
            return ()

        when (commands /= []) $ do
                let rawRequest = cs (unlines commands) :: ByteString
                resp2 <- post "http://localhost:3000" rawRequest
                return ()
        writeIORef stateRef' (loaded, False, changed, [])

        let rawRequest = cs "calculate_total_storage" :: ByteString
        resp3 <- post "http://localhost:3000" rawRequest

        return $ next (cs $ resp3 ^. responseBody)

    modifyQueue :: SmartExecutorState -> String -> IO ()
    modifyQueue stateRef' command = do
        (loaded, load', _, queue) <- readIORef stateRef'
        writeIORef stateRef' (loaded, load', True, queue ++ [command])

-- main :: IO ()
-- main = do
--   result <- runHttp testCommands
--   liftIO $ putStrLn result
--   return ()


main :: IO ()
main = do
  st <- newIORef (False, False, False, [])
  result <- runSmartExecutor st testCommands
  liftIO $ putStrLn result
  return ()


testCommands :: MyDomain String
testCommands = do
  load
  addComponent "storage(1,100)"
  load
  addComponent "production_unit(SolarPanel, 50)"
  addComponent "subsystem(storage(8,50)production_unit(WindTurbine, 30))"
  removeComponent 1
  setNextId 10
  facility <- showFacility
  production <- calculateTotalProduction
  storage <- calculateTotalStorage
  save
  addComponent "production_unit(SolarPanel, 50)"
  addComponent "subsystem(storage(8,50)production_unit(WindTurbine, 30))"

  return ("Facility: " ++ facility ++ "\nTotal Production: " ++ production ++ "\nTotal Storage: " ++ storage)


runCommandInMemory :: String -> IORef Lib2.State -> IO String
runCommandInMemory commandString st = do
    case Lib2.parse Lib3.parseCommand commandString of
        (Left e, _) -> do
            return ("Error parsing input: " ++ show e)

        (Right c, "") -> do
            case c of
                Lib3.StatementCommand (Lib3.Single query) -> do
                    extractedState <- readIORef st
                    case Lib2.stateTransition extractedState query of
                        Right (msg, updatedState) -> do
                            writeIORef st updatedState
                            case msg of
                                Just str -> return str
                                Nothing -> return "Success"
                        Left err -> do
                            return ("Error in state transition: " ++ show err)
                _ -> return "Invalid command"

        (Right _, r) -> do
            return ("Remaining input: " ++ show r)


runTest :: MyDomain a -> IO a
runTest program = do
    stateRef <- newIORef Lib2.emptyState
    run program stateRef
  where
    run :: MyDomain a -> IORef Lib2.State -> IO a
    run (Pure result) _ = return result
    run (Free step) stateRef = do
        next <- interpret step stateRef
        run next stateRef

    interpret :: MyDomainAlgebra a -> IORef Lib2.State -> IO a
    interpret (Load next) stateRef = do
        fileData <- SIO.readFile "./testData"
        _ <- runCommandInMemory fileData stateRef
        return $ next ()

    interpret (AddComponent component next) stateRef = do
        _ <- runCommandInMemory ("add(" ++ component ++ ")") stateRef
        return $ next ()

    interpret (RemoveComponent n next) stateRef = do
        _ <- runCommandInMemory ("remove(" ++ show n ++ ")") stateRef
        return $ next ()

    interpret (SetNextId n next) stateRef = do
        _ <- runCommandInMemory ("set_next_id(" ++ show n ++ ")") stateRef
        return $ next ()

    interpret (ShowFacility next) stateRef = do
        facilityState <- readIORef stateRef
        let facilityDescription = show facilityState
        return $ next facilityDescription

    interpret (CalculateTotalProduction next) stateRef = do
        production <- runCommandInMemory "calculate_total_production" stateRef
        return $ next production

    interpret (CalculateTotalStorage next) stateRef = do
        storage <- runCommandInMemory "calculate_total_storage" stateRef
        return $ next storage

    interpret (Save next) stateRef = do
        facilityState <- readIORef stateRef
        SIO.writeFile "./testData" (Lib3.renderStatements (Lib3.marshallState facilityState))
        return $ next ()
