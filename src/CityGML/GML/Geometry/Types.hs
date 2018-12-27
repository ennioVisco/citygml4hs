{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.GML.Geometry.Types

   Maintainer : Ennio Visconti ("ennio.visconti\@mail.polimi.it")
   Stability  : stable
   Portability: portable

   Types related to the GML model of CityGML.

   Note: Combined geometries can be aggregates, complexes or composites of
   primitives.

   1. __/Aggregates/__ have no restricted spatial relation between components.
   They are implemented in CityGML by the "Multi" prefix.
   2. __/Complexes/__ are topologically structured: their parts must be
   disjoint, must not overlap and are allowed to touch, at most, at their
   boundaries or
   share parts of their boundaries.
   3. __/Composites/__ are a special kind of complexes. They only contain
   elements of the same dimension. Additionally, their elements must be
   topologically connected among their boundaries.

-}

-- ------------------------------------------------------------

module CityGML.GML.Geometry.Types where

import           GHC.Generics
import           Identifiable

import           CityGML.GML.Feature.Types
import           CityGML.XLink.Types

-- ........................:::::::: _Geometry ::::::::...................... --

data Geometry =
        GC GeometricComplex
    |   GP GeometricPrimitive
    |   GA GeometricAggregate
    deriving (Read, Show, Eq, Generic, Identifiable)

-- .....................:::::::: GeometricComplex :::::..................... --

newtype GeometricComplex = GeometricComplex [GeometricPrimitive]
    deriving (Read, Show, Eq, Generic, Identifiable)

-- ...............:::::::: _AbstractGeometricAggregate :::::................ --

data GeometricAggregate =
        MGE MultiGeometry
    |   MSO MultiSolid
    |   MSU MultiSurface
    |   MCU MultiCurve
    |   MPO MultiPoint
    deriving (Read, Show, Eq, Generic, Identifiable)

-- ...................:::::::: GeometricAggregates ::::::::................. --

newtype MultiGeometry = MultiGeometry [Geometry]
    deriving (Read, Show, Eq, Generic, Identifiable)

newtype MultiSolid = MultiSolid [Solid]
    deriving (Read, Show, Eq, Generic, Identifiable)

data MultiSurface = MultiSurface
    {   msuFeature  :: Feature
    ,   msuSurfaces :: [Surface]
    }
    deriving (Read, Show, Eq, Generic, Identifiable)

newtype MultiCurve = MultiCurve [Curve]
    deriving (Read, Show, Eq, Generic, Identifiable)

newtype MultiPoint = MultiPoint [Point]
    deriving (Read, Show, Eq, Generic, Identifiable)

-- ...................:::::::: GeometricPrimitive ::::::::.................. --

data GeometricPrimitive =
        SO Solid
    |   SU Surface
    |   CU Curve
    |   PO Point
    deriving (Read, Show, Eq, Generic, Identifiable)

-- .........................:::::::: _Solid ::::::::........................ --
data Solid =
        Solid
        {   sdExterior ::  Surface
        ,   sdInterior :: [Surface]
        }
    |   CompositeSolid [Solid]
    deriving (Read, Show, Eq, Generic, Identifiable)

-- ........................:::::::: _Surface ::::::::....................... --

data Surface =
        Polygon
        {   scFeature  ::  Feature
        ,   scExterior ::  Ring
        ,   scInterior :: [Ring]
        }
    |   CompositeSurface [Surface]
    |   Surface [SurfacePatch]
    |   OrientableSurface
        {   sOrientation :: String
        ,   baseSurface  :: Surface
        }
    | SuLink XLink
    deriving (Read, Show, Eq, Generic, Identifiable)

data SurfacePatch = T Triangle | R Rectangle
    deriving (Read, Show, Eq, Generic, Identifiable)

newtype TriangulatedSurface = Patches [Triangle]
    deriving (Read, Show, Eq, Generic, Identifiable)

newtype Triangle = Triangle Ring
    deriving (Read, Show, Eq, Generic, Identifiable)

newtype Rectangle = Rectangle Ring
    deriving (Read, Show, Eq, Generic, Identifiable)

data Ring = LinearRing
    {   rFeature :: Feature
    ,   rPoints  :: [Point]
    }
    deriving (Read, Show, Eq, Generic, Identifiable)

-- .........................:::::::: _Curve ::::::::........................ --

data Curve =
        LineString [Point]
    |   CompositeCurve [Curve]
    deriving (Read, Show, Eq, Generic, Identifiable)

-- ..........................:::::::: Point ::::::::........................ --

data Point =
        Coord
        {   pCoords     :: String
        ,   pDimensions :: Maybe Int
        }
    |   List
        {   pList        :: String
        ,   plDimensions :: Maybe Int
        }
    deriving (Read, Show, Eq, Generic, Identifiable)
