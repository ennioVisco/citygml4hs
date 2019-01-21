{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

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
import           GHC.Generics
import           Identifiable

data TransportationObject =
        TCRO Road
    |   TCRA Railway
    |   TCSQ Square
    |   TCTR Track
    |   TA TrafficArea
    |   AT AuxiliaryTrafficArea
    deriving (Read, Show, Eq, Generic, Identifiable)

data TransportationComplex = TransportationComplex
    {   tcObject               :: CityObject
    ,   tcLod0Network          :: Maybe GeometricComplex
    ,   tcLod1Model            :: Maybe MultiSurface
    ,   tcData                 :: TransportationData
    ,   tcTrafficArea          :: [TrafficArea]
    ,   tcAuxiliaryTrafficArea :: [AuxiliaryTrafficArea]
    } deriving (Read, Show, Eq, Generic, Identifiable)

data TrafficArea = TrafficArea
    {   taObject :: CityObject
    ,   taData   :: TransportationData
    } deriving (Read, Show, Eq, Generic, Identifiable)

data AuxiliaryTrafficArea = AuxiliaryTrafficArea
    {   ataObject :: CityObject
    ,   ataData   :: TransportationData
    }
    deriving (Read, Show, Eq, Generic, Identifiable)

data TransportationData = TransportationData
    {   tranClass           :: Maybe String
    ,   tranFunction        :: Maybe String
    ,   tranUsage           :: Maybe String
    ,   tranSurfaceMaterial :: Maybe String
    ,   tranLod2Model       :: Maybe MultiSurface
    ,   tranLod3Model       :: Maybe MultiSurface
    ,   tranLod4Model       :: Maybe MultiSurface
    }   deriving (Read, Show, Eq, Generic, Identifiable)

data Road = Road
    {   roTranComplex   :: TransportationComplex
    }   deriving (Read, Show, Eq, Generic, Identifiable)

data Railway = Railway
    {   raTranComplex   :: TransportationComplex
    }   deriving (Read, Show, Eq, Generic, Identifiable)

data Square = Square
    {   sTranComplex   :: TransportationComplex
    }   deriving (Read, Show, Eq, Generic, Identifiable)

data Track = Track
    {   tTranComplex   :: TransportationComplex
    }   deriving (Read, Show, Eq, Generic, Identifiable)

data TranLod1Model = TranLod1MultiSurf MultiSurface
                 deriving (Read, Show, Eq, Generic, Identifiable)

data TranLod2Model = TranLod2MultiSurf MultiSurface
                 deriving (Read, Show, Eq, Generic, Identifiable)

data TranLod3Model = TranLod3MultiSurf MultiSurface
                 deriving (Read, Show, Eq, Generic, Identifiable)

data TranLod4Model = TranLod4MultiSurf MultiSurface
                 deriving (Read, Show, Eq, Generic, Identifiable)
