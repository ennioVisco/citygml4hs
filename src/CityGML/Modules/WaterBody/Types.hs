{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.Modules.WaterBody.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Types related to the WaterBody module of the Thematic model of CityGML.

-}

-- ------------------------------------------------------------

module CityGML.Modules.WaterBody.Types where

import           CityGML.GML.Types

import           GHC.Generics
import           Data.Data
import           Data.Binary
import           Identifiable


data WaterObject = WaterBody
    {   wtrFeature   :: Feature
    ,   wtrLod1Model :: Maybe WtrLod1Model
    }   deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)


data WtrLod1Model = WtrLod1MultiSurf MultiSurface
                 deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)
