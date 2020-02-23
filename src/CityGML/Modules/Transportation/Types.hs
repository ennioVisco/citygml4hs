{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.Modules.Transportation.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Types related to the Transportation module of the Thematic model of CityGML.

-}

-- ------------------------------------------------------------

module CityGML.Modules.Transportation.Types where

import           CityGML.Core.Types
import           CityGML.GML.Types
import           Data.Data
import           Data.Binary
import           GHC.Generics
import           Identifiable

data TransportationObject =
        TCRO Road
    |   TCRA Railway
    |   TCSQ Square
    |   TCTR Track
    |   TA TrafficArea
    |   AT AuxiliaryTrafficArea
    deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data TransportationComplex = TransportationComplex
    {   tcLod0Network          :: Maybe GeometricComplex
    ,   tcLod1Model            :: Maybe MultiSurface
    ,   tcData                 :: TransportationData
    ,   tcTrafficArea          :: [TrafficArea]
    ,   tcAuxiliaryTrafficArea :: [AuxiliaryTrafficArea]
    } deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data TrafficArea = TrafficArea
    {   taObject :: CityObject
    ,   taData   :: TransportationData
    } deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data AuxiliaryTrafficArea = AuxiliaryTrafficArea
    {   ataObject :: CityObject
    ,   ataData   :: TransportationData
    }
    deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data TransportationData = TransportationData
    {   tranClass           :: Maybe CodeType
    ,   tranFunction        :: [CodeType]
    ,   tranUsage           :: [CodeType]
    ,   tranSurfaceMaterial :: Maybe CodeType
    ,   tranLod2Model       :: Maybe MultiSurface
    ,   tranLod3Model       :: Maybe MultiSurface
    ,   tranLod4Model       :: Maybe MultiSurface
    }   deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data Road = Road
    {   roObject      :: CityObject
    ,   roTranComplex :: TransportationComplex
    }   deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data Railway = Railway
    {   raObject      :: CityObject
    ,   raTranComplex :: TransportationComplex
    }   deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data Square = Square
    {   sObject      :: CityObject
    ,   sTranComplex :: TransportationComplex
    }   deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data Track = Track
    {   tObject      :: CityObject
    ,   tTranComplex :: TransportationComplex
    }   deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data TranLod1Model = TranLod1MultiSurf MultiSurface
                 deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data TranLod2Model = TranLod2MultiSurf MultiSurface
                 deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data TranLod3Model = TranLod3MultiSurf MultiSurface
                 deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data TranLod4Model = TranLod4MultiSurf MultiSurface
                 deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)
