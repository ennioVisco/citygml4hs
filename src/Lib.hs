module Lib
    ( someFunc,
      module CityGML.Parsers,
      module CityGML.Types
    ) where

import           CityGML.Parsers
import           CityGML.Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"
