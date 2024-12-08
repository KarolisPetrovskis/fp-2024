module Lib2
  ( Query (..),
    Lib2.State (..),
    Component (..),
    EnergyProductionUnitType (..),
    ComponentWithId (..),
    emptyState,
    stateTransition,
    parseComponent,
    parseStorage,
    parseProductionUnit,
    parseSubsystem,
    parseQuery,
    parseAddComponent,
    parseRemoveComponent,
    parseShowFacility,
    parseLiteral,
    many,
    or3',
    Parser,
    strip,
    parse,
  )
where

import Control.Applicative (optional, (<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State.Strict as S
import Data.Char (isDigit)

-- | Data types for Queries and State
data Query
  = AddComponent Component
  | RemoveComponent Int
  | SetNextId Int
  | ShowFacility
  | CalculateTotalProduction
  | CalculateTotalStorage
  deriving (Eq, Show)

data EnergyProductionUnitType = SolarPanel | NuclearPlant | HydroPlant | WindTurbine
  deriving (Eq, Show)

data Component
  = Storage Double Double
  | ProductionUnit EnergyProductionUnitType Double
  | Subsystem [Component]
  deriving (Eq, Show)

data ComponentWithId = ComponentWithId
  { componentId :: Int,
    component :: Component
  }
  deriving (Eq, Show)

data State = State
  { facility :: [ComponentWithId],
    nextId :: Int
  }
  deriving (Eq, Show)

-- | Initial program state.
emptyState :: Lib2.State
emptyState = State {facility = [], nextId = 1}

-- | Basic parsers
-- type Parser a = String -> Either String (a, String)
type Parser a = ExceptT String (S.State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = S.runState (runExceptT parser)

many :: Parser a -> Parser [a]
many p =
  ( do
      x <- p
      xs <- many p
      return (x : xs)
  )
    `catchE` \_ -> return []

and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f p1 p2 = do
  v1 <- p1
  f v1 <$> p2

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f p1 p2 p3 = do
  v1 <- p1
  v2 <- p2
  f v1 v2 <$> p3

and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' f p1 p2 p3 p4 = do
  v1 <- p1
  v2 <- p2
  v3 <- p3
  f v1 v2 v3 <$> p4

and6' :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
and6' f p1 p2 p3 p4 p5 p6 = do
  v1 <- p1
  v2 <- p2
  v3 <- p3
  v4 <- p4
  v5 <- p5
  f v1 v2 v3 v4 v5 <$> p6

or2' :: Parser a -> Parser a -> Parser a
or2' a b = do
  inputBefore <- lift get
  a `catchE` \e1 -> do
    lift (put inputBefore)
    b `catchE` \e2 -> do
      lift (put inputBefore)
      throwE (e1 ++ "\n" ++ e2)

or3' :: Parser a -> Parser a -> Parser a -> Parser a
or3' a b c = do
  inputBefore <- lift get
  a `catchE` \e1 -> do
    lift (put inputBefore)
    b `catchE` \e2 -> do
      lift (put inputBefore)
      c `catchE` \e3 -> do
        lift (put inputBefore)
        throwE (e1 ++ "\n" ++ e2 ++ "\n" ++ e3)

or5' :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or5' a b c d e = do
  or3' a b c `catchE` \e1 -> do
    resultB <- or2' d e
    return resultB `catchE` \_ -> throwE e1

or6' :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or6' a b c d e f = do
  or3' a b c `catchE` \e1 -> do
    resultB <- or3' d e f
    return resultB `catchE` \_ -> throwE e1

-- borrowed permanentaly from https://hackage.haskell.org/package/MissingH-1.6.0.1/docs/src/Data.String.Utils.html#strip
strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip s = case s of
  [] -> []
  (x : xs) ->
    if x `elem` " \t\r\n"
      then lstrip xs
      else s

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

parseChar :: Char -> Parser Char
parseChar c = do
  input <- lift get
  case input of
    [] -> throwE ("Cannot find " ++ [c] ++ " in an empty input")
    s@(h : t) -> if c == h then lift $ put t >> return h else throwE (c : " is not found in " ++ s ++ "\n")

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  input <- lift get
  let strippedInput = strip input
  lift $ put strippedInput
  _ <- parseChar x
  rest <- parseLiteral xs
  return (x : rest)

parseString :: Parser String
parseString = do
  input <- lift get
  let input' = strip input
  case input' of
    ('"' : xs) -> parseQuotedString xs
    _ -> do
      let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
      lift (put rest)
      return str
  where
    parseQuotedString [] = throwE "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = lift (put rest) >> return ""
    parseQuotedString (x : rest) = do
      str <- parseQuotedString rest
      return (x : str)

parseInt :: Parser Int
parseInt = do
  input <- lift get
  let strippedInput = strip input
  let (digits, rest) = span isDigit strippedInput
  if null digits
    then throwE "Expected an integer"
    else do
      lift (put rest)
      return (read digits)

parseDouble :: Parser Double
parseDouble = do
  input <- lift get
  let strippedInput = strip input
  let (digits, rest) = span (\c -> isDigit c || c == '.') strippedInput
  if null digits
    then throwE "Expected a double"
    else do
      lift (put rest)
      return (read digits)

parseEnergyProductionUnitType :: Parser EnergyProductionUnitType
parseEnergyProductionUnitType = do
  str <- parseString
  case str of
    "SolarPanel" -> return SolarPanel
    "NuclearPlant" -> return NuclearPlant
    "HydroPlant" -> return HydroPlant
    "WindTurbine" -> return WindTurbine
    _ -> throwE "Unknown energy production unit type"

-- <energy_production_unit> ::= <energy_production_unit_type> <production>
parseProductionUnit :: Parser Component
parseProductionUnit =
  and6'
    (\_ _ uType _ production _ -> ProductionUnit uType production)
    (parseLiteral "production_unit")
    (parseChar '(')
    parseEnergyProductionUnitType
    (parseChar ',')
    parseDouble
    (parseChar ')')

-- <storage> ::= "storage" "(" <efficiency> "," <max_storage_amount> ")"
parseStorage :: Parser Component
parseStorage =
  and6'
    (\_ _ efficiency _ storage _ -> Storage efficiency storage)
    (parseLiteral "storage")
    (parseChar '(')
    parseDouble
    (parseChar ',')
    parseDouble
    (parseChar ')')

-- <component> ::= <energy_production_unit> | <storage> | <subsystem>
parseComponent :: Parser Component
parseComponent =
  or3'
    parseStorage
    parseProductionUnit
    parseSubsystem

-- <subsystem> ::= "subsystem" "(" component+ ")"
parseSubsystem :: Parser Component
parseSubsystem =
  and4'
    (\_ _ components _ -> Subsystem components)
    (parseLiteral "subsystem")
    (parseChar '(')
    (many parseComponent)
    (parseChar ')')

-- <AddComponent> ::= "add" <component>
parseAddComponent :: Parser Query
parseAddComponent =
  and4'
    (\_ _ component' _ -> AddComponent component')
    (parseLiteral "add")
    (parseChar '(')
    parseComponent
    (parseChar ')')

-- <RemoveComponent> ::= "remove" <Number>
parseRemoveComponent :: Parser Query
parseRemoveComponent =
  and4'
    (\_ _ componentId' _ -> RemoveComponent componentId')
    (parseLiteral "remove")
    (parseChar '(')
    parseInt
    (parseChar ')')

-- ShowFacility ::= "show_facility"
parseShowFacility :: Parser Query
parseShowFacility = do
  _ <- parseLiteral "show_facility"
  return ShowFacility

-- CalculateTotalProduction ::= "calculate_total_production"
parseCalculateTotalProduction :: Parser Query
parseCalculateTotalProduction = do
  _ <- parseLiteral "calculate_total_production"
  return CalculateTotalProduction

-- CalculateTotalStorage ::= "calculate_total_storage"
parseCalculateTotalStorage :: Parser Query
parseCalculateTotalStorage = do
  _ <- parseLiteral "calculate_total_storage"
  return CalculateTotalStorage

parseSetNextId :: Parser Query
parseSetNextId =
  and4'
    (\_ _ n _ -> SetNextId n)
    (parseLiteral "set_next_id")
    (parseLiteral "(")
    parseInt
    (parseLiteral ")")

parseQuery :: Parser Query
parseQuery =
  or6'
    parseAddComponent
    parseRemoveComponent
    parseShowFacility
    parseCalculateTotalProduction
    parseCalculateTotalStorage
    parseSetNextId

stateTransition :: Lib2.State -> Query -> Either String (Maybe String, Lib2.State)
stateTransition st query = case query of
  SetNextId n ->
    let newState = st {facility = (facility st), nextId = n}
     in Right (Just ("Next component will have ID " ++ show n), newState)
  AddComponent component' ->
    let newId = (nextId st)
        newComponentWithId = ComponentWithId newId component'
        updatedFacility = newComponentWithId : facility st
        newState = st {facility = updatedFacility, nextId = newId + 1}
     in if any (\c -> componentId c == newId) (facility st)
          then
            Left ("Component with id " ++ show newId ++ " already exists")
          else
            Right (Just ("Added " ++ show newComponentWithId), newState)
  RemoveComponent compId ->
    let existingComponent = filter (\c -> componentId c == compId) (facility st)
     in if not (null existingComponent)
          then
            let newFacility = filter (\c -> componentId c /= compId) (facility st)
                newState = st {facility = newFacility}
             in Right (Just ("Removed component with id " ++ show compId), newState)
          else Left ("Component with id " ++ show compId ++ " not found")
  ShowFacility ->
    let facilityList = facility st
        facilityStr = unlines $ map show facilityList
     in Right (Just ("Current facility components:\n" ++ facilityStr), st)
  CalculateTotalProduction ->
    let totalProduction = calculateTotalFacilityProduction st
     in Right (Just ("Total production: " ++ show totalProduction), st)
  CalculateTotalStorage ->
    let totalCapacity = calculateTotalFacilityStorage st
     in Right (Just ("Total storage capacity: " ++ show totalCapacity), st)

calculateTotalProduction :: [Component] -> Double
calculateTotalProduction [] = 0
calculateTotalProduction (c : cs) = case c of
  ProductionUnit _ production -> production + calculateTotalProduction cs
  Storage _ _ -> calculateTotalProduction cs
  Subsystem components -> calculateTotalProduction components + calculateTotalProduction cs

calculateTotalStorage :: [Component] -> Double
calculateTotalStorage [] = 0
calculateTotalStorage (c : cs) = case c of
  Storage _ capacity ->
    let totalCap = calculateTotalStorage cs
     in capacity + totalCap
  ProductionUnit _ _ -> calculateTotalStorage cs
  Subsystem components ->
    let subCap = calculateTotalStorage components
        restCap = calculateTotalStorage cs
     in subCap + restCap

calculateTotalFacilityProduction :: Lib2.State -> Double
calculateTotalFacilityProduction st = calculateTotalProduction (map component (facility st))

calculateTotalFacilityStorage :: Lib2.State -> Double
calculateTotalFacilityStorage st = calculateTotalStorage (map component (facility st))
