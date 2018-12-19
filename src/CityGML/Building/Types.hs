{-# LANGUAGE DeriveGeneric #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.Building.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Types related to the Building module of the Thematic model of CityGML.

-}

-- ------------------------------------------------------------

module CityGML.Building.Types where

import           CityGML.GML.Types

import           GHC.Generics

data AbstractBuilding = Building
    {   bFeature       :: Feature
    -- Building Optional Information
    ,   bHeight        :: Maybe Measure
    ,   bRoofType      :: Maybe String
    ,   bYearOfConstr  :: Maybe String
    ,   bFunction      :: Maybe String
    ,   bStAboveGround :: Maybe Int
    -- Building Models
    ,   bLod0FootPrint :: Maybe BldgLod0Model
    ,   bLod0RoofEdge  :: Maybe BldgLod0Model
    ,   bLod1Solid     :: Maybe BldgLod1Model
    ,   bLod3Solid     :: Maybe BldgLod3Model
    -- Building External Interfaces
    ,   bInstallations :: [BuildingInstallation]
    ,   bBoundedBy     :: [BldgBoundary]
    }
    deriving (Read, Show, Eq, Generic)

data BldgLod0Model = FootPrint MultiSurface
               | RoofEdge MultiSurface
                deriving (Read, Show, Eq, Generic)

data BldgLod1Model = BldgLod1Solid Solid
                 deriving (Read, Show, Eq, Generic)


data BldgLod3Model = BldgLod3Multi MultiSurface
                 deriving (Read, Show, Eq, Generic)

data BldgBoundary =
        Wall    WallSurface
    |   Closure BuildingSurface
    |   Roof    RoofSurface
    |   Ground  BuildingSurface
    deriving (Read, Show, Eq, Generic)

data WallSurface = WallSurface
    {   wlFeature   :: Feature
    ,   wlLod3Model :: BldgLod3Model
    ,   wlOpenings  :: [Opening]
    } deriving (Read, Show, Eq, Generic)


data RoofSurface = RoofSurface
    {   rfFeature   :: Feature
    ,   rfLod3Model :: BldgLod3Model
    ,   rfOpenings  :: [Opening]
    } deriving (Read, Show, Eq, Generic)

data Opening =
        Door
        {   dFeature   :: Feature
        ,   dLod3Model :: BldgLod3Model
        }
    |   Window
        {   wFeature   :: Feature
        ,   wLod3Model :: BldgLod3Model
        } deriving (Read, Show, Eq, Generic)

data BuildingSurface = BuildingSurface
    {   bsFeature   :: Feature
    ,   bsLod3Model :: BldgLod3Model
    } deriving (Read, Show, Eq, Generic)

newtype BuildingInstallation = BldgInstallation [BldgBoundary]
    deriving (Read, Show, Eq, Generic)

data Measure = Height
    {   mUom   :: String
    ,   mValue :: Float
    }   deriving (Read, Show, Eq, Generic)
