{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.Modules.Vegetation.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Types related to the Vegetation module of the Thematic model of CityGML.

-}

-- ------------------------------------------------------------

module CityGML.Modules.Vegetation.Types where

import           CityGML.GML.Types

import           GHC.Generics
import           Data.Data
import           Identifiable

data VegetationObject = PlantCover
    {   vegFeature   :: Feature
    ,   vegLod1Model :: Maybe VegLod1Model
    }   deriving (Read, Show, Eq, Data, Generic, Identifiable)


data VegLod1Model = VegLod1MultiSurf MultiSurface
                 deriving (Read, Show, Eq, Data, Generic, Identifiable)
