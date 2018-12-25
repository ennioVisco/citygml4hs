-- ------------------------------------------------------------

{- |
   Module     : CityGML.Modules.Building.Parsers

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Parsers (i.e. 'XMLPickler's) related to the Types defined in
   'CityGML.Modules.Building.Types'.

-}

-- ------------------------------------------------------------

module CityGML.Modules.Building.Parsers where

import           CityGML.Core.Parsers             (xpCityObject)
import           CityGML.GML.Parsers
import           CityGML.Modules.Building.Types
import           CityGML.Modules.Generics.Parsers (xpGenericAttribute)
import           Text.XML.HXT.Core


instance XmlPickler BldgLod0Model where
    xpickle = xpBldgLod0

instance XmlPickler BldgLod1Model where
    xpickle = xpBldgLod1

instance XmlPickler BldgLod2Model where
    xpickle = xpBldgLod2

instance XmlPickler BldgLod3Model where
    xpickle = xpBldgLod3

instance XmlPickler AbstractBuilding where
    xpickle = xpBuilding

instance XmlPickler BuildingInstallation where
    xpickle = xpBldgInst

instance XmlPickler WallSurface where
    xpickle = xpWallSurface

instance XmlPickler RoofSurface where
    xpickle = xpRoofSurface

instance XmlPickler BuildingSurface where
    xpickle = xpBuildingSurface

instance XmlPickler Opening where
    xpickle = xpOpening

instance XmlPickler BldgBoundary where
    xpickle = xpBldgBoundary

instance XmlPickler Measure where
    xpickle = xpMeasure

xpBldgLod0 :: PU BldgLod0Model
xpBldgLod0
    = xpAlt tag ps
        where
        tag (FootPrint _) = 0
        tag (RoofEdge  _) = 1
        ps = [  xpWrap  ( FootPrint
                        , \ (FootPrint ms) -> ms
                        )
                (xpElem "bldg:lod0FootPrint" xpickle)
             ,  xpWrap  ( RoofEdge
                             , \ (RoofEdge ms) -> ms
                             )
                (xpElem "bldg:lod0RoofEdge"  xpickle)
             ]


xpBldgLod1 :: PU BldgLod1Model
xpBldgLod1
    = xpAlt tag ps
        where
        tag (BldgLod1Solid _) = 0
        ps = [  xpWrap  ( BldgLod1Solid
                        , \ (BldgLod1Solid s) -> s
                        ) $
                xpElem "bldg:lod1Solid" xpSolid
             ]

xpBldgLod2 :: PU BldgLod2Model
xpBldgLod2
    = xpAlt tag ps
        where
        tag (BldgLod2Solid _) = 0
        ps = [  xpWrap  ( BldgLod2Solid
                        , \ (BldgLod2Solid s) -> s
                        ) $
                xpElem "bldg:lod2Solid" xpSolid
             ]

xpBldgLod3 :: PU BldgLod3Model
xpBldgLod3
    = xpAlt tag ps
        where
        tag (BldgLod3Multi _) = 0
        ps = [  xpWrap  ( BldgLod3Multi
                        , \ (BldgLod3Multi s) -> s
                        ) $
                xpElem "bldg:lod3MultiSurface" xpMultiSurface
             ]


xpBuilding :: PU AbstractBuilding
xpBuilding =
    xpElem "bldg:Building"    $
    xpWrap  (\(g, e, f,r,h,y,s, l0f,l0r,l1,l2,l3, b,i) ->
                Building g  e  f r h y s  l0f l0r l1 l2 l3  b i
            , \ b ->    (   bObject        b
                        -- Extra Generic Attributes
                        ,   bExtras        b
                        -- Building Optional Information
                        ,   bFunction      b
                        ,   bRoofType      b
                        ,   bHeight        b
                        ,   bYearOfConstr  b
                        ,   bStAboveGround b
                        -- Building Models
                        ,   bLod0FootPrint b
                        ,   bLod0RoofEdge  b
                        ,   bLod1Solid     b
                        ,   bLod2Solid     b
                        ,   bLod3Solid     b
                        -- Building External Interfaces
                        ,   bInstallations b
                        ,   bBoundedBy     b
                        )
            ) $
    xp14Tuple   xpCityObject
                -- Extra Generic Attributes
                (xpList xpGenericAttribute)
                -- Building Optional Information
                (xpOption $ xpElem "bldg:function"           xpText)
                (xpOption $ xpElem "bldg:roofType"           xpText)
                (xpOption xpMeasure)
                (xpOption $ xpElem "bldg:yearOfConstruction" xpText)
                (xpOption $ xpElem "bldg:storeysAboveGround" xpPrim)
                -- Building Models
                (xpOption xpBldgLod0)
                (xpOption xpBldgLod0)
                (xpOption xpBldgLod1)
                (xpOption xpBldgLod2)
                (xpOption xpBldgLod3)
                -- Building External Interfaces
                (xpList $ xpElem "bldg:outerBuildingInstallation" xpBldgInst)
                (xpList $ xpElem "bldg:boundedBy" xpBldgBoundary)

xpBldgBoundary :: PU BldgBoundary
xpBldgBoundary
  = xpAlt tag ps
        where
        tag (Wall    _) = 0
        tag (Closure _) = 1
        tag (Roof    _) = 2
        tag (Ground  _) = 3
        ps = [  xpWrap  ( Wall
                        , \ (Wall w) -> w
                        ) $
                xpElem "bldg:WallSurface"    xpWallSurface
            ,   xpWrap  ( Closure
                        , \ (Closure w) -> w
                        ) $
                xpElem "bldg:ClosureSurface" xpBuildingSurface
            ,   xpWrap  ( Roof
                        , \ (Roof w) -> w
                        ) $
                xpElem "bldg:RoofSurface"    xpRoofSurface
            ,   xpWrap  ( Ground
                        , \ (Ground w) -> w
                        ) $
                xpElem "bldg:GroundSurface"  xpBuildingSurface
           ]

xpBldgInst :: PU BuildingInstallation
xpBldgInst
  = xpElem "bldg:BuildingInstallation" $
    xpElem "bldg:lod3Geometry" $
    xpWrap  ( BldgInstallation
            , \ (BldgInstallation s) -> s
            ) $
    xpList $ xpElem "bldg:boundedBy" xpBldgBoundary

xpWallSurface :: PU WallSurface
xpWallSurface
 =  xpWrap  ( uncurry3 WallSurface
            , \s -> (wlFeature s, wlLod3Model s, wlOpenings s)
            ) $
    xpTriple    xpFeature
                xpBldgLod3
                (xpList $ xpElem "bldg:opening" xpOpening)

xpRoofSurface :: PU RoofSurface
xpRoofSurface
 =  xpWrap  ( uncurry3 RoofSurface
            , \s -> (rfFeature s, rfLod3Model s, rfOpenings s)
            ) $
    xpTriple    xpFeature
                xpBldgLod3
                (xpList $ xpElem "bldg:opening" xpOpening)

xpOpening :: PU Opening
xpOpening
 =  xpAlt tag ps
       where
       tag (Door   _ _) = 0
       tag (Window _ _) = 1
       ps = [   xpElem  "bldg:Door" $
                xpWrap  ( uncurry Door
                        , \d -> (dFeature d, dLod3Model d)
                        ) $
                xpPair  xpFeature
                        xpBldgLod3
           ,    xpElem  "bldg:Window" $
                xpWrap  ( uncurry Window
                        , \w -> (wFeature w, wLod3Model w)
                        ) $
                xpPair  xpFeature
                        xpBldgLod3
          ]


xpBuildingSurface :: PU BuildingSurface
xpBuildingSurface
 =  xpWrap  ( uncurry BuildingSurface
            , \s -> (bsFeature s, bsLod3Model s)) $
    xpPair  xpFeature
            xpBldgLod3

xpMeasure :: PU Measure
xpMeasure
  = xpElem "bldg:measuredHeight" $
    xpWrap  ( uncurry Height
            , \m -> (mUom m, mValue m)) $
    xpPair  (xpAttr "uom" xpText)
            xpPrim
