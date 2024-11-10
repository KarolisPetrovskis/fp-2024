{-# LANGUAGE InstanceSigs #-}

module Lib3
  ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    applyQueries,
    getQueries,
  )
where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)
import Control.Concurrent.STM.TVar
import Data.Maybe (fromMaybe)
import Lib2 (parseLiteral)
import qualified Lib2

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
fileName :: String
fileName = "stateBackup.txt"

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of
    Save content replyChan -> do
      writeFile fileName content
      writeChan replyChan ()
    Load replyChan -> do
      content <- readFile fileName
      writeChan replyChan content
  storageOpLoop chan

data Statements
  = Batch [Lib2.Query]
  | Single Lib2.Query
  deriving (Show, Eq)

data Command
  = StatementCommand Statements
  | LoadCommand
  | SaveCommand
  deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input =
  case parseLiteral "load" input of
    Right (_, rest) -> Right (LoadCommand, rest)
    Left _ -> case parseLiteral "save" input of
      Right (_, rest) -> Right (SaveCommand, rest)
      Left _ -> case parseStatements input of
        Right (statements, rest) -> Right (StatementCommand statements, rest)
        Left err -> Left $ "Failed to parse command\n" ++ err

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input = do
  case Lib2.many Lib2.parseQuery input of
    Right ([], _) -> Left ""
    Right (queries, "") ->
      if length queries == 1
        then Right (Single (head queries), "")
        else Right (Batch queries, "")
    Right (_, _) -> Left " One of the queries failed"
    Left err -> Left err

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState st =
  let (lc : _) = Lib2.facility st
      lastId = Lib2.componentId lc
   in if null (Lib2.facility st) || lastId == ((Lib2.nextId st) - 1)
        then Batch (componentsToQueriesWrapper (reverse (Lib2.facility st)))
        else Batch (componentsToQueriesWrapper (reverse (Lib2.facility st)) ++ [Lib2.SetNextId (Lib2.nextId st)])

componentsToQueriesWrapper :: [Lib2.ComponentWithId] -> [Lib2.Query]
componentsToQueriesWrapper components =
  let (id', queries) = componentsToQueries components
   in if id' /= 1
        then Lib2.SetNextId id' : queries
        else queries

componentsToQueries :: [Lib2.ComponentWithId] -> (Int, [Lib2.Query])
componentsToQueries [] = (1, [])
componentsToQueries (c : cs) =
  let (id', q) = componentToQuery c
      (nextId, qs) = componentsToQueries cs
   in if nextId == id' + 1 || null qs
        then (id', q : qs)
        else (id', q : Lib2.SetNextId nextId : qs)

-- components >>= componentToQuery

componentToQuery :: Lib2.ComponentWithId -> (Int, Lib2.Query)
componentToQuery compWithId =
  let component = Lib2.component compWithId
      id' = Lib2.componentId compWithId
   in (id', Lib2.AddComponent component)

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file.
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderStatements (Batch [query])
renderStatements (Batch queries) = unlines $ map getStringFromQuery queries

getStringFromQuery :: Lib2.Query -> String
getStringFromQuery q = do
  case q of
    Lib2.AddComponent component -> "add(" ++ getStringFromComponent component ++ ")"
    Lib2.SetNextId id' -> "set_next_id(" ++ show id' ++ ")"
    _ -> "How did we get here?"

getStringFromComponent :: Lib2.Component -> String
getStringFromComponent (Lib2.Storage a b) = "storage(" ++ show a ++ ", " ++ show b ++ ")"
getStringFromComponent (Lib2.ProductionUnit a b) = "production_unit(" ++ show a ++ ", " ++ show b ++ ")"
getStringFromComponent (Lib2.Subsystem components) = "subsystem(" ++ unwords (map getStringFromComponent components) ++ ")"

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition ::
  TVar Lib2.State ->
  Command ->
  Chan StorageOp ->
  IO (Either String (Maybe String))
stateTransition st command ioChan = case command of
  LoadCommand -> do
    replyChan <- newChan
    writeChan ioChan (Load replyChan)
    content <- readChan replyChan
    case parseStatements content of
      Left err -> return $ Left err
      Right (statements, _) -> atomically $ do
        let (_, newState) = applyQueries Lib2.emptyState (getQueries statements)
        writeTVar st newState
        return $ Right $ Just "State loaded successfully."
  SaveCommand -> do
    currentState <- readTVarIO st
    result <- saveStateToFile currentState ioChan
    return $ case result of
      Left err -> Left err
      Right () -> Right $ Just "State saved successfully."
  StatementCommand statements -> atomically $ do
    currentState <- readTVar st
    let (msg, newState) = applyQueries currentState (getQueries statements)
    writeTVar st newState
    return $ Right $ Just ("Commands parsed\nLog:\n" ++ msg)

saveStateToFile :: Lib2.State -> Chan StorageOp -> IO (Either String ())
saveStateToFile state ioChan = do
  let serializedState = renderStatements (marshallState state)
  replyChan <- newChan
  writeChan ioChan (Save serializedState replyChan)
  -- Wait for the operation to complete
  result <- readChan replyChan
  return $ Right result

getQueries :: Statements -> [Lib2.Query]
getQueries (Single query) = [query]
getQueries (Batch queries) = queries

applyQueries :: Lib2.State -> [Lib2.Query] -> (String, Lib2.State)
applyQueries st [] = ("", st)
applyQueries st (q : qs) =
  let (msg, newState) = applyQuery st q
      (otherMsg, finalState) = applyQueries newState qs
   in (msg ++ "\n" ++ otherMsg, finalState)

applyQuery :: Lib2.State -> Lib2.Query -> (String, Lib2.State)
applyQuery state query =
  case Lib2.stateTransition state query of
    Right (msg, newState) -> (fromMaybe "" msg, newState)
    Left e -> (e, state)
