{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.Modules.Building.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Types related to the Building module of the Thematic model of CityGML.

-}

-- ------------------------------------------------------------

module CityGML.Modules.Building.Types where

import           CityGML.Core.Types
import           CityGML.GML.Types
import           CityGML.Modules.Generics.Types
import           Identifiable

import           GHC.Generics

data AbstractBuilding =
        Building BuildingData
    |   BuildingPart BuildingData
    deriving (Read, Show, Eq, Generic, Identifiable)

data BuildingData = BldgData
    {   bObject        :: CityObject
    -- Extra Generic Attributes
    ,   bExtras        :: [GenericAttribute]

    ,   bInfo          :: BuildingInfo
    ,   bModels        :: BuildingModels
    ,   bIntersection  :: BuildingIntersections
    -- Building External Interfaces
    ,   bInstallations :: [BuildingInstallation]
    ,   bBoundedBy     :: [BldgBoundary]
    ,   bBuildingParts :: [AbstractBuilding]
    ,   bAddress       :: Maybe Address
    } deriving (Read, Show, Eq, Generic, Identifiable)

data BuildingIntersections = BuildingIntersections
    {   bLod1TerrainInt :: Maybe BldgLod1Int
    ,   bLod2TerrainInt :: Maybe BldgLod2Int
    } deriving (Read, Show, Eq, Generic, Identifiable)

-- Building Optional Information
data BuildingInfo = BuildingInfo
    {   bFunction      :: Maybe String
    ,   bRoofType      :: Maybe String
    ,   bHeight        :: Maybe Measure
    ,   bYearOfConstr  :: Maybe String
    ,   bStAboveGround :: Maybe Int
    } deriving (Read, Show, Eq, Generic, Identifiable)

-- Building Models
data BuildingModels = BuildingModels
    {   bLod0FootPrint :: Maybe BldgLod0Model
    ,   bLod0RoofEdge  :: Maybe BldgLod0Model
    ,   bLod1Solid     :: Maybe BldgLod1Model
    ,   bLod2Solid     :: Maybe BldgLod2Model
    ,   bLod3Solid     :: Maybe BldgLod3Model
    } deriving (Read, Show, Eq, Generic, Identifiable)

data BldgLod0Model = FootPrint MultiSurface
               | RoofEdge MultiSurface
                deriving (Read, Show, Eq, Generic, Identifiable)

newtype BldgLod1Int = BldgLod1Int MultiCurve
                 deriving (Read, Show, Eq, Generic, Identifiable)

newtype BldgLod2Int = BldgLod2Int MultiCurve
                 deriving (Read, Show, Eq, Generic, Identifiable)

newtype BldgLod1Model = BldgLod1Solid Solid
                 deriving (Read, Show, Eq, Generic, Identifiable)

data BldgLod2Model =
        BldgLod2Solid Solid
    |   BldgLod2Multi MultiSurface
    deriving (Read, Show, Eq, Generic, Identifiable)


newtype BldgLod3Model = BldgLod3Multi MultiSurface
                 deriving (Read, Show, Eq, Generic, Identifiable)

data BldgBoundary =
        Wall    WallSurface
    |   Closure BuildingSurface
    |   Roof    RoofSurface
    |   Ground  BuildingSurface
    deriving (Read, Show, Eq, Generic, Identifiable)

data WallSurface = WallSurface
    {   wlObject    :: CityObject
    ,   wlLod2Model :: Maybe BldgLod2Model
    ,   wlLod3Model :: Maybe BldgLod3Model
    ,   wlOpenings  :: [Opening]
    } deriving (Read, Show, Eq, Generic, Identifiable)


data RoofSurface = RoofSurface
    {   rfObject    :: CityObject
    ,   rfLod2Model :: Maybe BldgLod2Model
    ,   rfLod3Model :: Maybe BldgLod3Model
    ,   rfOpenings  :: [Opening]
    } deriving (Read, Show, Eq, Generic, Identifiable)

data BuildingSurface = BuildingSurface
    {   bsObject    :: CityObject
    ,   bsLod2Model :: Maybe BldgLod2Model
    ,   bsLod3Model :: Maybe BldgLod3Model
    } deriving (Read, Show, Eq, Generic, Identifiable)

data Opening =
        Door
        {   dObject    :: CityObject
        ,   dLod3Model :: BldgLod3Model
        }
    |   Window
        {   wObject    :: CityObject
        ,   wLod3Model :: BldgLod3Model
        } deriving (Read, Show, Eq, Generic, Identifiable)

newtype BuildingInstallation = BldgInstallation [BldgBoundary]
    deriving (Read, Show, Eq, Generic, Identifiable)

data Measure = Height
    {   mUom   :: String
    ,   mValue :: Float
    }   deriving (Read, Show, Eq, Generic, Identifiable)
