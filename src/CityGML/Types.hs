
-- ------------------------------------------------------------

{- |
   Module     : CityGML.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Wrapper for all the types of the CityGML package

-}

-- ------------------------------------------------------------

module CityGML.Types
    (
    -- Export CityGML Modules
    module Bridge,
    module Building,
    module Core,
    module Generics,
    module GML,
    module Relief,
    module Transportation,
    module Vegetation,
    module WaterBody
    )
where

-- Import CityGML Modules
import           CityGML.Core.Strategy                as Core
import           CityGML.GML.Types                    as GML
import           CityGML.XLink.Types                  as XLink

import           CityGML.Modules.Bridge.Types         as Bridge
import           CityGML.Modules.Building.Types       as Building
import           CityGML.Modules.Generics.Types       as Generics
import           CityGML.Modules.Relief.Types         as Relief
import           CityGML.Modules.Transportation.Types as Transportation
import           CityGML.Modules.Vegetation.Types     as Vegetation
import           CityGML.Modules.WaterBody.Types      as WaterBody
