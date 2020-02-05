{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

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

import           Data.Data
import           GHC.Generics
import           Identifiable

import           CityGML.GML.Base
import           CityGML.XLink.Types

-- ........................:::::::: _Geometry ::::::::...................... --
data AbstractGeometry = AbstractGeometry
    {   gGml         :: GML
    ,   srsReference :: Maybe SRSReferenceGroup
    } deriving (Read, Show, Eq, Data, Generic, Identifiable)

data Geometry =
        GC GeometricComplex
    |   GP GeometricPrimitive
    |   GA GeometricAggregate
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

-- .....................:::::::: GeometricComplex :::::..................... --

newtype GeometricComplex = GeometricComplex [GeometricPrimitive]
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

-- ...............:::::::: _AbstractGeometricAggregate :::::................ --

data GeometricAggregate =
        MGE MultiGeometry
    |   MSO MultiSolid
    |   MSU MultiSurface
    |   MCU MultiCurve
    |   MPO MultiPoint
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

-- ...................:::::::: GeometricAggregates ::::::::................. --

newtype MultiGeometry = MultiGeometry [Geometry]
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

newtype MultiSolid = MultiSolid [Solid]
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

data MultiSurface = MultiSurface
    {   msuAbstractGeometry :: AbstractGeometry
    ,   msuSurfaces         :: [Surface]
    }
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

newtype MultiCurve = MultiCurve [Curve]
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

newtype MultiPoint = MultiPoint [Point]
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

-- ...................:::::::: GeometricPrimitive ::::::::.................. --

data GeometricPrimitive =
        SO Solid
    |   SU Surface
    |   CU Curve
    |   PO Point
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

-- .........................:::::::: _Solid ::::::::........................ --
data Solid =
        Solid
        {   sdAbstractGeometry ::  AbstractGeometry
        ,   sdExterior         ::  Surface
        ,   sdInterior         :: [Surface]
        }
    |   CompositeSolid [Solid]
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

-- ........................:::::::: _Surface ::::::::....................... --

data Surface =
        Polygon
        {   scAbstractGeometry ::  AbstractGeometry
        ,   scExterior         ::  Ring
        ,   scInterior         :: [Ring]
        }
    |   CompositeSurface
        {   csAbstractGeometry :: AbstractGeometry
        ,   csMembers          :: [Surface]
        }
    |   Surface [SurfacePatch]
    |   OrientableSurface
        {   sOrientation :: String
        ,   baseSurface  :: Surface
        }
    | SuLink XLink
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

data SurfacePatch = T Triangle | R Rectangle
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

newtype TriangulatedSurface = Patches [Triangle]
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

newtype Triangle = Triangle Ring
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

newtype Rectangle = Rectangle Ring
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

data Ring = LinearRing
    {   rAbstractGeometry :: AbstractGeometry
    ,   rPoints           :: [Point]
    }
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

-- .........................:::::::: _Curve ::::::::........................ --

data Curve =
        LineString [Point]
    |   CompositeCurve [Curve]
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

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
    deriving (Read, Show, Eq, Data, Generic, Identifiable)
