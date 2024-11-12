module Lib2
  ( Query (..),
    State (..),
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
  )
where

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
emptyState :: State
emptyState = State {facility = [], nextId = 1}

-- | Basic parsers
type Parser a = String -> Either String (a, String)

many :: Parser a -> Parser [a]
many p = many' p []
  where
    many' p' acc input =
      case p' input of
        Left _ -> Right (acc, input)
        Right (v, r) -> many' p' (acc ++ [v]) r

and2' ::
  (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f p1 p2 input =
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) -> Right (f v1 v2, r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f p1 p2 p3 input =
  case and2' (,) p1 p2 input of
    Right ((v1, v2), r2) ->
      case p3 r2 of
        Right (v3, r3) -> Right (f v1 v2 v3, r3)
        Left e2 -> Left e2
    Left e1 -> Left e1

and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' f p1 p2 p3 p4 input =
  case and2' (,) p1 p2 input of
    Right ((v1, v2), r2) ->
      case and2' (,) p3 p4 r2 of
        Right ((v3, v4), r4) -> Right (f v1 v2 v3 v4, r4)
        Left e2 -> Left e2
    Left e1 -> Left e1

and6' :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
and6' f p1 p2 p3 p4 p5 p6 input =
  case and3' (,,) p1 p2 p3 input of
    Right ((v1, v2, v3), r3) ->
      case and3' (,,) p4 p5 p6 r3 of
        Right ((v4, v5, v6), r6) -> Right (f v1 v2 v3 v4 v5 v6, r6)
        Left e2 -> Left e2
    Left e1 -> Left e1

or2' :: Parser a -> Parser a -> Parser a
or2' p1 p2 input =
  case p1 input of
    Right (v1, r1) -> Right (v1, r1)
    Left e1 ->
      case p2 input of
        Right (v2, r2) -> Right (v2, r2)
        Left e2 -> Left e2

or3' :: Parser a -> Parser a -> Parser a -> Parser a
or3' p1 p2 p3 input =
  case or2' p1 p2 input of
    Right (v1, r1) -> Right (v1, r1)
    Left e1 ->
      case p3 input of
        Right (v3, r3) -> Right (v3, r3)
        Left e2 -> Left e2

or5' :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or5' p1 p2 p3 p4 p5 input =
  case or3' p1 p2 p3 input of
    Right (v1, r1) -> Right (v1, r1)
    Left e1 ->
      case or2' p4 p5 input of
        Right (v4, r4) -> Right (v4, r4)
        Left e2 -> Left e2

or6' :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or6' p1 p2 p3 p4 p5 p6 input =
  case or3' p1 p2 p3 input of
    Right (v1, r1) -> Right (v1, r1)
    Left e1 ->
      case or3' p4 p5 p6 input of
        Right (v4, r4) -> Right (v4, r4)
        Left e2 -> Left e2

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
parseChar _ [] = Left "Unexpected end of input"
parseChar c input =
  let input' = strip input
   in if null input'
        then Left "Unexpected end of input"
        else
          if head input' == c
            then Right (c, tail input')
            else Left $ "Expected '" ++ [c] ++ "', but found '" ++ [head input'] ++ "'"

parseLiteral :: String -> Parser String
parseLiteral [] input = Right ([], input)
parseLiteral (x : xs) input =
  let input' = strip input
   in if null input'
        then Left "Unexpected end of input"
        else
          if head input' == x
            then case parseLiteral xs (tail input') of
              Right (str, rest) -> Right (x : str, rest)
              Left err -> Left err
            else Left $ "Expected " ++ (x : xs) ++ ", but found " ++ take (length (x : xs)) input'

parseString :: Parser String
parseString input =
  let input' = strip input
   in if null input'
        then Right ("", "")
        else
          if head input' == '"'
            then parseQuotedString (tail input')
            else
              let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
               in Right (str, rest)
  where
    parseQuotedString [] = Left "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = Right ("", rest)
    parseQuotedString (x : rest) = case parseQuotedString rest of
      Right (str, rest') -> Right (x : str, rest')
      Left err -> Left err

parseInt :: Parser Int
parseInt input =
  let (digits, rest) = span isDigit (strip input)
   in if null digits
        then Left "Expected an integer"
        else Right (read digits, rest)

parseDouble :: Parser Double
parseDouble input =
  let (digits, rest) = span (\c -> isDigit c || c == '.') (strip input)
   in if null digits
        then Left "Expected a double"
        else Right (read digits, rest)

-- <energy_production_unit_type> ::= "solar_panel" | "nuclear_plant" | "hydro_plant" | "wind_turbine"
parseEnergyProductionUnitType :: Parser EnergyProductionUnitType
parseEnergyProductionUnitType input = case parseString (strip input) of
  Right ("SolarPanel", rest) -> Right (SolarPanel, rest)
  Right ("NuclearPlant", rest) -> Right (NuclearPlant, rest)
  Right ("HydroPlant", rest) -> Right (HydroPlant, rest)
  Right ("WindTurbine", rest) -> Right (WindTurbine, rest)
  _ -> Left "Failed to parse production unit type"

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
parseShowFacility input =
  case parseLiteral "show_facility" (strip input) of
    Right (_, rest) -> Right (ShowFacility, rest)
    Left _ -> Left "Expected 'show_facility'"

-- CalculateTotalProduction ::= "calculate_total_production"
parseCalculateTotalProduction :: Parser Query
parseCalculateTotalProduction input =
  case parseLiteral "calculate_total_production" (strip input) of
    Right (_, rest) -> Right (CalculateTotalProduction, rest)
    Left _ -> Left "Expected 'calculate_total_production'"

-- CalculateTotalStorage ::= "calculate_total_storage"
parseCalculateTotalStorage :: Parser Query
parseCalculateTotalStorage input =
  case parseLiteral "calculate_total_storage" (strip input) of
    Right (_, rest) -> Right (CalculateTotalStorage, rest)
    Left _ -> Left "Expected 'calculate_total_storage'"

parseSetNextId :: Parser Query
parseSetNextId =
  and4'
    (\_ _ n _ -> SetNextId n)
    (parseLiteral "set_next_id")
    (parseLiteral "(")
    parseInt
    (parseLiteral ")")

parseQuery :: String -> Either String (Query, String)
parseQuery input =
  case or6' parseAddComponent parseRemoveComponent parseShowFacility parseCalculateTotalProduction parseCalculateTotalStorage parseSetNextId input of
    Right (query, rest) -> Right (query, rest)
    Left _ -> Left "Failed to parse: Unknown command"

stateTransition :: State -> Query -> Either String (Maybe String, State)
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

calculateTotalFacilityProduction :: State -> Double
calculateTotalFacilityProduction st = calculateTotalProduction (map component (facility st))

calculateTotalFacilityStorage :: State -> Double
calculateTotalFacilityStorage st = calculateTotalStorage (map component (facility st))
