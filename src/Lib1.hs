module Lib1
  ( completions,
  )
where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions =
  [ "facility",
    "component",
    "subsystem",
    "component_list",
    "energy_production_unit",
    "solar_panel",
    "nuclear_plant",
    "hydro_plant",
    "wind_turbine",
    "max_storage_amount"
  ]