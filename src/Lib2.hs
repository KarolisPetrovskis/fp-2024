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
  )
where

import Data.Char (isDigit)

-- | Data types for Queries and State
data Query
  = AddComponent Component
  | RemoveComponent Int
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

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

parseChar :: Char -> Parser Char
parseChar _ [] = Left "Unexpected end of input"
parseChar c input =
  let input' = skipSpaces input
   in if null input'
        then Left "Unexpected end of input"
        else
          if head input' == c
            then Right (c, tail input')
            else Left $ "Expected '" ++ [c] ++ "', but found '" ++ [head input'] ++ "'"

parseLiteral :: String -> Parser String
parseLiteral [] input = Right ([], input)
parseLiteral (x : xs) input =
  let input' = skipSpaces input
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
  let input' = skipSpaces input
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
  let (digits, rest) = span isDigit (skipSpaces input)
   in if null digits
        then Left "Expected an integer"
        else Right (read digits, rest)

parseDouble :: Parser Double
parseDouble input =
  let (digits, rest) = span (\c -> isDigit c || c == '.') (skipSpaces input)
   in if null digits
        then Left "Expected a double"
        else Right (read digits, rest)

-- <energy_production_unit_type> ::= "solar_panel" | "nuclear_plant" | "hydro_plant" | "wind_turbine"
parseEnergyProductionUnitType :: Parser EnergyProductionUnitType
parseEnergyProductionUnitType input = case parseString (skipSpaces input) of
  Right ("SolarPanel", rest) -> Right (SolarPanel, rest)
  Right ("NuclearPlant", rest) -> Right (NuclearPlant, rest)
  Right ("HydroPlant", rest) -> Right (HydroPlant, rest)
  Right ("WindTurbine", rest) -> Right (WindTurbine, rest)
  _ -> Left "Failed to parse production unit type"

-- <energy_production_unit> ::= <energy_production_unit_type> <production>
parseProductionUnit :: Parser Component
parseProductionUnit input =
  case parseLiteral "production_unit" input of
    Left err -> Left err
    Right (_, rest) ->
      case parseChar '(' rest of
        Left err -> Left err
        Right (_, rest1) ->
          case parseEnergyProductionUnitType rest1 of
            Left err -> Left err
            Right (uType, rest2) ->
              case parseChar ',' rest2 of
                Left err -> Left err
                Right (_, rest3) ->
                  case parseDouble rest3 of
                    Left err -> Left err
                    Right (production, rest4) ->
                      case parseChar ')' rest4 of
                        Left err -> Left err
                        Right (_, rest5) ->
                          Right (ProductionUnit uType production, rest5)

-- <storage> ::= "storage" "(" <efficiency> "," <max_storage_amount> ")"
parseStorage :: Parser Component
parseStorage input =
  case parseLiteral "storage" input of
    Left err -> Left err
    Right (_, rest) ->
      case parseChar '(' rest of
        Left err -> Left err
        Right (_, rest1) ->
          case parseDouble rest1 of
            Left err -> Left err
            Right (efficiency, rest2) ->
              case parseChar ',' rest2 of
                Left err -> Left err
                Right (_, rest3) ->
                  case parseDouble rest3 of
                    Left err -> Left err
                    Right (maxStorage, rest4) ->
                      case parseChar ')' rest4 of
                        Left err -> Left err
                        Right (_, rest5) ->
                          Right (Storage efficiency maxStorage, rest5)

-- <component> ::= <energy_production_unit> | <storage> | <subsystem>
parseComponent :: Parser Component
parseComponent input =
  case parseStorage input of
    Right result -> Right result
    Left err1 -> case parseProductionUnit input of
      Right result -> Right result
      Left err2 ->
        case parseSubsystem input of
          Right result -> Right result
          Left err3 -> Left $ err1 ++ err2 ++ err3

-- <subsystem> ::= "subsystem" "(" component+ ")"
parseSubsystem :: Parser Component
parseSubsystem input =
  case parseLiteral "subsystem" input of
    Left err -> Left err
    Right (_, rest) ->
      case parseChar '(' rest of
        Left err -> Left err
        Right (_, rest1) ->
          case many parseComponent rest1 of
            Left err -> Left err
            Right (components, rest2) ->
              case parseChar ')' rest2 of
                Left err -> Left err
                Right (_, rest3) ->
                  Right (Subsystem components, rest3)

-- <AddComponent> ::= "add" <component>
parseAddComponent :: Parser Query
parseAddComponent input =
  case parseLiteral "add" input of
    Left err -> Left err
    Right (_, rest) ->
      case parseChar '(' rest of
        Left err -> Left err
        Right (_, rest1) ->
          case parseComponent rest1 of
            Left err -> Left err
            Right (component', rest2) ->
              case parseChar ')' rest2 of
                Left err -> Left err
                Right (_, rest3) ->
                  Right (AddComponent component', rest3)

-- <RemoveComponent> ::= "remove" <Number>
parseRemoveComponent :: Parser Query
parseRemoveComponent input =
  case parseLiteral "remove" input of
    Left err -> Left err
    Right (_, rest) ->
      case parseChar '(' rest of
        Left err -> Left err
        Right (_, rest1) ->
          case parseInt rest1 of
            Left err -> Left err
            Right (componentId', rest2) ->
              case parseChar ')' rest2 of
                Left err -> Left err
                Right (_, rest3) ->
                  Right (RemoveComponent componentId', rest3)

-- ShowFacility ::= "show_facility"
parseShowFacility :: Parser Query
parseShowFacility input =
  case parseLiteral "show_facility" (skipSpaces input) of
    Right (_, rest) -> Right (ShowFacility, rest)
    Left _ -> Left "Expected 'show_facility'"

-- CalculateTotalProduction ::= "calculate_total_production"
parseCalculateTotalProduction :: Parser Query
parseCalculateTotalProduction input =
  case parseLiteral "calculate_total_production" (skipSpaces input) of
    Right (_, rest) -> Right (CalculateTotalProduction, rest)
    Left _ -> Left "Expected 'calculate_total_production'"

-- CalculateTotalStorage ::= "calculate_total_storage"
parseCalculateTotalStorage :: Parser Query
parseCalculateTotalStorage input =
  case parseLiteral "calculate_total_storage" (skipSpaces input) of
    Right (_, rest) -> Right (CalculateTotalStorage, rest)
    Left _ -> Left "Expected 'calculate_total_storage'"

-- >>> parseQuery "add (storage(1, 2))"
-- NOW Right (AddComponent (Storage 1.0 2))
parseQuery :: String -> Either String Query
parseQuery input =
  case parseAddComponent input of
    Right (query, _) -> Right query
    Left err1 -> case parseRemoveComponent input of
      Right (query, _) -> Right query
      Left err2 -> case parseShowFacility input of
        Right (query, _) -> Right query
        Left err3 -> case parseCalculateTotalProduction input of
          Right (query, _) -> Right query
          Left err4 -> case parseCalculateTotalStorage input of
            Right (query, _) -> Right query
            Left err5 -> Left $ "Failed to parse query: " ++ err1 ++ "; " ++ err2 ++ "; " ++ err3 ++ "; " ++ err4 ++ "; " ++ err5

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  AddComponent component' ->
    let newComponentWithId = ComponentWithId (nextId st) component'
        updatedFacility = newComponentWithId : facility st
        newState = st {facility = updatedFacility, nextId = nextId st + 1}
     in Right (Just ("Added " ++ show newComponentWithId), newState)
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
