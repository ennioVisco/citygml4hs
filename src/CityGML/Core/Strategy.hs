{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.Core.Strategy

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Strategy for switching from CityObject to concrete objects.

-}

-- ------------------------------------------------------------

module CityGML.Core.Strategy
    (   module CityGML.Core.Types
    ,   CityModel(..)
    ,   CityObjectMember(..)
    ,   Site(..)
    ,   xpCityModel
    ,   xpCityObjectMember
    ,   xpSite
    )
where

import           GHC.Generics
import           Data.Data
import           Identifiable
import           Text.XML.HXT.Core

import           CityGML.Core.Types
import           CityGML.GML.Parsers
import           CityGML.GML.Types
import           CityGML.Namespaces

import           CityGML.Modules.Bridge.Types           (AbstractBridge)
import           CityGML.Modules.Building.Types         (AbstractBuilding)
import           CityGML.Modules.Generics.Types         (GenericCityObject)
import           CityGML.Modules.Relief.Types           (ReliefFeature)
import           CityGML.Modules.Transportation.Types   (TransportationObject)
import           CityGML.Modules.Vegetation.Types       (VegetationObject)
import           CityGML.Modules.WaterBody.Types        (WaterObject)


import           CityGML.Modules.Bridge.Parsers
import           CityGML.Modules.Building.Parsers
import           CityGML.Modules.Generics.Parsers
import           CityGML.Modules.Relief.Parsers
import           CityGML.Modules.Transportation.Parsers
import           CityGML.Modules.Vegetation.Parsers
import           CityGML.Modules.WaterBody.Parsers

data CityModel = CityModel
    {   cFeature :: Feature
    ,   cMembers :: [CityObjectMember]
    }   deriving (Read, Show, Eq, Data, Generic, Identifiable)

data CityObjectMember =
        Site Site
    |   Veg  VegetationObject
    |   Gen  GenericCityObject
    |   Wtr  WaterObject
    |   Tran TransportationObject
    |   Dem  ReliefFeature
    --    Grp  CityObjectGroup
    --    Frn  CityFurniture
    --    Luse LandUse
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

data Site =
        Bld AbstractBuilding
    |   Brg AbstractBridge
    --    Tun AbstractTunnel
    deriving (Read, Show, Eq, Data, Generic, Identifiable)



instance XmlPickler CityModel where
    xpickle = xpCityModel

instance XmlPickler CityObjectMember where
    xpickle = xpCityObjectMember

instance XmlPickler Site where
    xpickle = xpSite

xpCityModel :: PU CityModel
xpCityModel
    = xpElem "CityModel" $
      xpNamespaces namespaces $
      xpWrap    ( uncurry CityModel
                , \ c -> (cFeature c, cMembers c)
                ) $
      xpPair    xpFeature
                (xpList xpCityObjectMember)



xpCityObjectMember :: PU CityObjectMember
xpCityObjectMember
  = xpElem "cityObjectMember" $
    xpAlt tag ps
        where
        tag (Site _) = 0
        tag (Veg  _) = 1
        tag (Gen  _) = 2
        tag (Wtr  _) = 3
        tag (Tran _) = 4
        tag (Dem  _) = 5
        ps =    [   xpWrap  ( Site
                            , \ (Site s) -> s
                            )
                    xpSite

                ,   xpWrap  ( Veg
                            , \ (Veg v) -> v
                            )
                    xpVegetation

                ,   xpWrap  ( Gen
                            , \ (Gen g) -> g
                            )
                    xpGenerics

                ,   xpWrap  ( Wtr
                            , \ (Wtr w) -> w
                            )
                    xpWaterBody

                ,   xpWrap  ( Tran
                            , \ (Tran t) -> t
                            )
                    xpTransportation

                ,   xpWrap  ( Dem
                            , \ (Dem r) -> r
                            )
                    xpReliefFeature
                ]

xpSite :: PU Site
xpSite
  = xpAlt tag ps
      where
      tag (Bld _) = 0
      tag (Brg _) = 1
      ps =    [  xpWrap  ( Bld
                          , \ (Bld b) -> b
                          )
                  xpBuilding

              ,   xpWrap  ( Brg
                          , \ (Brg b) -> b
                          )
                  xpBridge
              ]



-- | Extra Special Picklers
xpNamespaces :: [(String, String)] -> PU a -> PU a
xpNamespaces xs v = foldr (uncurry xpAddFixedAttr) v xs
