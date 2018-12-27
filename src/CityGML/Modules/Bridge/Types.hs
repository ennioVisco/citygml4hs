{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.Modules.Bridge.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Types related to the Bridge module of the Thematic model of CityGML.

-}

-- ------------------------------------------------------------

module CityGML.Modules.Bridge.Types where

import           CityGML.GML.Types

import           GHC.Generics
import           Identifiable

data AbstractBridge = Bridge
    {   brgFeature   :: Feature
    ,   brgLod1Model :: Maybe BrgLod1Model
    }   deriving (Read, Show, Eq, Generic, Identifiable)


data BrgLod1Model = BrgLod1MultiSurf MultiSurface
                 deriving (Read, Show, Eq, Generic, Identifiable)
