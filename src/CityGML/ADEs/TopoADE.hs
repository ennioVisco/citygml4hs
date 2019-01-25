{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}

module CityGML.ADEs.TopoADE where

import           Data.Tree.NTree.TypeDefs
import           GHC.Generics
import           Data.Data
import           Identifiable
import           Text.XML.HXT.Core

newtype RelationSet = Rels [TopoRelation]
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

data TopoRelation = Near String [TopoNode]
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

data TopoNode = TopoBuilding String
    deriving (Read, Show, Eq, Data, Generic, Identifiable)

instance XmlPickler RelationSet where
    xpickle = xpRelSet

instance XmlPickler TopoRelation where
    xpickle = xpRelation

instance XmlPickler TopoNode where
    xpickle = xpAlt tag ps
        where
        tag (TopoBuilding _) = 0
        ps = [  xpWrap  ( TopoBuilding
                        , \ (TopoBuilding i) -> i
                        )
                (   xpElem "topology:topoBuilding" $
                    xpAttr "xlink:href" xpText
                )
             ]

xpRelSet :: PU RelationSet
xpRelSet =  xpElem "topology:relations"  $
            xpWrap  ( Rels
                    , \ (Rels rs) -> rs) $
            xpList xpRelation

xpRelation :: PU TopoRelation
xpRelation = xpAlt tag ps
    where
    tag (Near _ _) = 0
    ps = [  xpNear
         ]

xpNear :: PU TopoRelation
xpNear
  = xpElem "topology:topoRelation" $
    xpElem "topology:topoNear"     $
    xpWrap  ( uncurry Near
            , \ (Near i ns) -> (i, ns)) $
    xpPair (xpAttr "gml:id" xpText)
           (xpList xpickle)
