{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
import           Data.Data
import           Identifiable

data GenericCityObject = GenericCityObject
    {   genFeature   :: Feature
    ,   genLod1Model :: GenLod1Model
    }   deriving (Read, Show, Eq, Data, Generic, Identifiable)


newtype GenLod1Model = GenLod1Geometry MultiSurface
                 deriving (Read, Show, Eq, Data, Generic, Identifiable)

data GenericAttribute =
        StringAttribute
        {   gsaName  :: String
        ,   gsaValue :: Maybe String
        }
    |   IntAttribute
        {   giaName  :: String
        ,   giaValue :: String
        }
    |   DoubleAttribute
        {   gdaName  :: String
        ,   gdaValue :: String
        }
    deriving (Read, Show, Eq, Data, Generic, Identifiable)
