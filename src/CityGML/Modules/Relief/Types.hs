{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.Modules.Relief.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Types related to the Relief module of the Thematic model of CityGML.

-}

-- ------------------------------------------------------------

module CityGML.Modules.Relief.Types where

import           CityGML.GML.Types

import           GHC.Generics
import           Data.Data
import           Identifiable

data ReliefFeature = ReliefFeature
        {   demFFeature   :: Feature
        ,   demFLod       :: Int
        ,   demComponents :: [Relief]
        }
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

data ReliefComponent = ReliefComponent
        {   demCFeature :: Feature
        ,   demCLod     :: Int
        }
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

data Relief =
        TINRelief
        {   demTINComponent :: ReliefComponent
        ,   demTin          :: TriangulatedSurface
        }
    |   MassPointRelief
        {   demMPComponent :: ReliefComponent
        ,   reliefPoints   :: MultiPoint
        }
    |   BreaklineRelief
        {   demBLComponent     :: ReliefComponent
        ,   demBreaklines      :: Maybe MultiCurve
        ,   ridgeOrValleyLines :: Maybe MultiCurve
        }
    deriving (Read, Show, Eq, Data, Generic, Identifiable)
