{-# LANGUAGE DeriveGeneric #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.Modules.Generics.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Types related to the Generics module of the Thematic model of CityGML.

-}

-- ------------------------------------------------------------

module CityGML.Modules.Generics.Types where

import           CityGML.GML.Types

import           GHC.Generics

data GenericCityObject = GenericCityObject
    {   genFeature   :: Feature
    ,   genLod1Model :: GenLod1Model
    }   deriving (Read, Show, Eq, Generic)


newtype GenLod1Model = GenLod1Geometry MultiSurface
                 deriving (Read, Show, Eq, Generic)

data GenericAttribute =
        StringAttribute
        {   gaName  :: String
        ,   gaValue :: String
        } deriving (Read, Show, Eq, Generic)