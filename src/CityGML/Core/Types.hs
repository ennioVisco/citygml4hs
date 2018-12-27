{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.Core.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Types related to the Core module of the Thematic model of CityGML.

-}

-- ------------------------------------------------------------

module CityGML.Core.Types where

import           CityGML.GML.Types
import           CityGML.XAL.Types

import           GHC.Generics
import           Identifiable

data Address = Address XalAddressDetails
    deriving (Read, Show, Eq, Generic, Identifiable)

data ExternalReference = ExternalReference
    {   erInformationSystem :: Maybe String
    ,   erExternalObjRef    :: ExternalObject
    } deriving (Read, Show, Eq, Generic, Identifiable)

data ExternalObject = ExternalObject
    {   eoName :: String
    ,   eoUri  :: Maybe String
    } deriving (Read, Show, Eq, Generic, Identifiable)

data CityObject = CityObject
    {   oFeature           :: Feature
    ,   oCreationDate      ::  Maybe String
    ,   oTerminationDate   ::  Maybe String
    ,   oExternalReference ::  [ExternalReference]
    ,   oGeneralizesTo     ::  [CityObject]
    ,   oRelativeToTerrain ::  Maybe String
    ,   oRelativeToWater   ::  Maybe String
    } deriving (Read, Show, Eq, Generic, Identifiable)
