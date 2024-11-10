{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.List (nubBy)
import Debug.Trace
import Lib2 qualified
import Lib3 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary, arbitrary)
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

removeDuplicateComponents :: Lib2.State -> Lib2.State
removeDuplicateComponents state =
  let uniqueFacility = nubBy (\c1 c2 -> Lib2.componentId c1 == Lib2.componentId c2) (Lib2.facility state)
   in state {Lib2.facility = uniqueFacility}

positiveDoubleWithTwoDecimals :: Gen Double
positiveDoubleWithTwoDecimals = do
  value <- (arbitrary :: Gen Double) `suchThat` (> 0)
  let roundedValue = fromIntegral (round (value * 10)) / 10
  return roundedValue

instance Arbitrary Lib2.State where
  arbitrary :: Gen Lib2.State
  arbitrary = do
    components <- arbitrary
    nextId <- arbitrary `suchThat` (> 0)
    return $ Lib2.State components nextId

instance Arbitrary Lib2.ComponentWithId where
  arbitrary :: Gen Lib2.ComponentWithId
  arbitrary = do
    componentId <- arbitrary `suchThat` (> 0)
    Lib2.ComponentWithId componentId <$> arbitrary

instance Arbitrary Lib2.Component where
  arbitrary :: Gen Lib2.Component
  arbitrary =
    oneof
      [ Lib2.Storage <$> positiveDoubleWithTwoDecimals <*> positiveDoubleWithTwoDecimals,
        Lib2.ProductionUnit <$> arbitrary <*> positiveDoubleWithTwoDecimals,
        Lib2.Subsystem <$> arbitrary
      ]

instance Arbitrary Lib2.EnergyProductionUnitType where
  arbitrary :: Gen Lib2.EnergyProductionUnitType
  arbitrary =
    oneof
      [ pure Lib2.SolarPanel,
        pure Lib2.NuclearPlant,
        pure Lib2.HydroPlant,
        pure Lib2.WindTurbine
      ]

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Lib2 tests"
    [ testCase "Parse Add Component - Production Unit" $
        Lib2.parseQuery "add (production_unit (SolarPanel, 12))"
          @?= Right (Lib2.AddComponent (Lib2.ProductionUnit Lib2.SolarPanel 12), ""),
      testCase "Parse Add Component - Storage" $
        Lib2.parseQuery "add (storage (5.5, 100.0))"
          @?= Right (Lib2.AddComponent (Lib2.Storage 5.5 100.0), ""),
      testCase "Parse Add Component - Subsystem" $
        Lib2.parseQuery "add (subsystem (storage (1.5, 50) production_unit (WindTurbine, 10)))"
          @?= Right (Lib2.AddComponent (Lib2.Subsystem [Lib2.Storage 1.5 50, Lib2.ProductionUnit Lib2.WindTurbine 10]), ""),
      testCase "Parse Remove Component" $
        Lib2.parseQuery "remove (3)"
          @?= Right (Lib2.RemoveComponent 3, ""),
      testCase "Parse Show Facility" $
        Lib2.parseQuery "show_facility"
          @?= Right (Lib2.ShowFacility, ""),
      testCase "Parse Calculate Total Production" $
        Lib2.parseQuery "calculate_total_production"
          @?= Right (Lib2.CalculateTotalProduction, ""),
      testCase "Parse Calculate Total Storage" $
        Lib2.parseQuery "calculate_total_storage"
          @?= Right (Lib2.CalculateTotalStorage, ""),
      testCase "Parse Next Id" $
        Lib2.parseQuery "set_next_id(5)"
          @?= Right (Lib2.SetNextId 5, ""),
      testCase "Parse Nonsense" $
        Lib2.parseQuery "Nonsense"
          @?= Left "Failed to parse: Unknown command"
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "Lib3 tests"
    [ QC.testProperty "marshallState produces statements that reconstruct state" $
        withMaxSuccess 3 $
          \initialState ->
            let trueInitialState = removeDuplicateComponents initialState
                statements = Lib3.marshallState trueInitialState
                renderedStatements = Lib3.renderStatements statements
             in case Lib3.parseStatements renderedStatements of
                  Right (parsedStatements, "") ->
                    let queries = Lib3.getQueries parsedStatements
                        (_, reconstructedState) = Lib3.applyQueries Lib2.emptyState queries
                     in reconstructedState == trueInitialState
                  _ -> False
    ]
