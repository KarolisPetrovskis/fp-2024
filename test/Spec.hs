{-# LANGUAGE ImportQualifiedPost #-}

import Lib2 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib2 tests"
    [ testCase "Parse Add Component - Production Unit" $
        Lib2.parseQuery "add (production_unit (SolarPanel, 12))"
          @?= Right (Lib2.AddComponent (Lib2.ProductionUnit Lib2.SolarPanel 12)),
      testCase "Parse Add Component - Storage" $
        Lib2.parseQuery "add (storage (5.5, 100.0))"
          @?= Right (Lib2.AddComponent (Lib2.Storage 5.5 100.0)),
      testCase "Parse Add Component - Subsystem" $
        Lib2.parseQuery "add (subsystem (storage (1.5, 50) production_unit (WindTurbine, 10)))"
          @?= Right (Lib2.AddComponent (Lib2.Subsystem [Lib2.Storage 1.5 50, Lib2.ProductionUnit Lib2.WindTurbine 10])),
      testCase "Parse Remove Component" $
        Lib2.parseQuery "remove (3)"
          @?= Right (Lib2.RemoveComponent 3),
      testCase "Parse Show Facility" $
        Lib2.parseQuery "show_facility"
          @?= Right Lib2.ShowFacility,
      testCase "Parse Calculate Total Production" $
        Lib2.parseQuery "calculate_total_production"
          @?= Right Lib2.CalculateTotalProduction,
      testCase "Parse Calculate Total Storage" $
        Lib2.parseQuery "calculate_total_storage"
          @?= Right Lib2.CalculateTotalStorage,
      testCase "Parse Nonsense" $
        Lib2.parseQuery "Nonsense"
          @?= Left "Failed to parse: Unknown command",
      testCase "State Transition - Add Production Unit" $
        let initialState = Lib2.emptyState
         in case Lib2.parseQuery "add (production_unit (SolarPanel, 15.0))" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Right (Just msg, newState) -> do
                    msg @?= "Added ComponentWithId {componentId = 1, component = ProductionUnit SolarPanel 15.0}"
                    length (Lib2.facility newState) @?= 1
                  Left err -> error err
              Left err -> error err,
      testCase "State Transition - Add Storage" $
        let initialState = Lib2.emptyState
         in case Lib2.parseQuery "add (storage (5.5, 100.0))" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Right (Just msg, newState) -> do
                    msg @?= "Added ComponentWithId {componentId = 1, component = Storage 5.5 100.0}"
                    length (Lib2.facility newState) @?= 1
                  Left err -> error err
              Left err -> error err,
      testCase "State Transition - Try Remove Non Existent Component" $
        let initialState = Lib2.State {Lib2.facility = [Lib2.ComponentWithId 1 (Lib2.Storage 5.5 100.0)], Lib2.nextId = 2}
         in case Lib2.parseQuery "remove (20)" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Left err -> do
                    err @?= "Component with id 20 not found"
                  Left err -> error err
              Left err -> error err,
      testCase "State Transition - Remove Component" $
        let initialState = Lib2.State {Lib2.facility = [Lib2.ComponentWithId 1 (Lib2.Storage 5.5 100.0)], Lib2.nextId = 2}
         in case Lib2.parseQuery "remove (1)" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Right (Just msg, newState) -> do
                    msg @?= "Removed component with id 1"
                    length (Lib2.facility newState) @?= 0
                  Left err -> error err
              Left err -> error err,
      testCase "State Transition - Show Facility" $
        let initialState = Lib2.State {Lib2.facility = [Lib2.ComponentWithId 1 (Lib2.Storage 5.5 100.0)], Lib2.nextId = 2}
         in case Lib2.parseQuery "show_facility" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Right (Just facilityStr, _) -> do
                    facilityStr @?= "Current facility components:\nComponentWithId {componentId = 1, component = Storage 5.5 100.0}\n"
                  Left err -> error err
              Left err -> error err,
      testCase "State Transition - Calculate Total Production" $
        let initialState = Lib2.State {Lib2.facility = [Lib2.ComponentWithId 1 (Lib2.ProductionUnit Lib2.SolarPanel 15.0)], Lib2.nextId = 2}
         in case Lib2.parseQuery "calculate_total_production" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Right (Just msg, _) -> do
                    msg @?= "Total production: 15.0"
                  Left err -> error err
              Left err -> error err,
      testCase "State Transition - Calculate Total Storage" $
        let initialState = Lib2.State {Lib2.facility = [Lib2.ComponentWithId 1 (Lib2.Storage 5.5 100.0)], Lib2.nextId = 2}
         in case Lib2.parseQuery "calculate_total_storage" of
              Right query ->
                case Lib2.stateTransition initialState query of
                  Right (Just msg, _) -> do
                    msg @?= "Total storage capacity: 100.0"
                  Left err -> error err
              Left err -> error err
    ]