-- ------------------------------------------------------------

{- |
   Module     : CityGML.Building.Parsers

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Parsers (i.e. 'XMLPickler's) related to the Types defined in
   'CityGML.Building.Types'.

-}

-- ------------------------------------------------------------

module CityGML.Building.Parsers where

import           CityGML.GML.Parsers
import           CityGML.Types
import           Text.XML.HXT.Core


instance XmlPickler BldgLod0Model where
    xpickle = xpBldgLod0

instance XmlPickler BldgLod1Model where
    xpickle = xpBldgLod1

instance XmlPickler BldgLod3Model where
    xpickle = xpBldgLod3

instance XmlPickler AbstractBuilding where
    xpickle = xpBuilding

instance XmlPickler BuildingInstallation where
    xpickle = xpBldgInst

instance XmlPickler WallSurface where
    xpickle = xpWallSurface

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
                (xpElem "bldg:Lod0FootPrint" xpickle)
             ,  xpWrap  ( RoofEdge
                             , \ (RoofEdge ms) -> ms
                             )
                (xpElem "bldg:Lod0RoofEdge"  xpickle)
             ]


xpBldgLod1 :: PU BldgLod1Model
xpBldgLod1
    = xpAlt tag ps
        where
        tag (BldgLod1Solid _) = 0
        ps = [  xpWrap  ( BldgLod1Solid
                        , \ (BldgLod1Solid s) -> s
                        ) $
                xpElem "bldg:Lod1Solid" xpSolid
             ]

xpBldgLod3 :: PU BldgLod3Model
xpBldgLod3
    = xpAlt tag ps
        where
        tag (BldgLod3Multi _) = 0
        ps = [  xpWrap  ( BldgLod3Multi
                        , \ (BldgLod3Multi s) -> s
                        ) $
                xpElem "bldg:Lod3MultiSurface" xpMultiSurface
             ]


xpBuilding :: PU AbstractBuilding
xpBuilding =
    xpElem "bldg:Building"    $
    xpWrap  (\(g, h,s,y,r,f, l0f,l0r,l1,l3, b,i) ->
                Building g  h s y r f  l0f l0r l1 l3  b i
            , \ b ->    (   bFeature b
                        -- Building Optional Information
                        ,   bHeight        b
                        ,   bStAboveGround b
                        ,   bYearOfConstr  b
                        ,   bRoofType      b
                        ,   bFunction      b
                        -- Building Models
                        ,   bLod0FootPrint b
                        ,   bLod0RoofEdge  b
                        ,   bLod1Solid     b
                        ,   bLod3Solid     b
                        -- Building External Interfaces
                        ,   bBoundedBy     b
                        ,   bInstallations b
                        )
            ) $
    xp12Tuple   xpFeature
                -- Building Optional Information
                (xpOption xpMeasure)
                (xpOption $ xpElem "bldg:roofType"           xpText)
                (xpOption $ xpElem "bldg:yearOfConstruction" xpText)
                (xpOption $ xpElem "bldg:function"           xpText)
                (xpOption $ xpElem "bldg:storeysAboveGround" xpPrim)
                -- Building Models
                (xpOption xpBldgLod0)
                (xpOption xpBldgLod0)
                (xpOption xpBldgLod1)
                (xpOption xpBldgLod3)
                -- Building External Interfaces
                (xpList $ xpElem "bldg:boundedBy" xpBldgBoundary)
                (xpList $ xpElem "bldg:outerBuildingInstallation" xpBldgInst)

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
                xpElem "bldg:WallSurface" xpWallSurface
            ,   xpWrap  ( Closure
                        , \ (Closure w) -> w
                        ) $
                xpElem "bldg:ClosureSurface" xpBuildingSurface
            ,   xpWrap  ( Roof
                        , \ (Roof w) -> w
                        ) $
                xpElem "bldg:RoofSurface" xpBuildingSurface
            ,   xpWrap  ( Ground
                        , \ (Ground w) -> w
                        ) $
                xpElem "bldg:GroundSurface" xpBuildingSurface
           ]

xpBldgInst :: PU BuildingInstallation
xpBldgInst
  = xpElem "bldg:BuildingInstallation" $
    xpElem "bldg:lod3Geometry" $
    xpWrap  ( BldgInstallation
            , \ (BldgInstallation s) -> s
            ) $
    xpList xpBldgBoundary

xpWallSurface :: PU WallSurface
xpWallSurface
 =  xpWrap  ( uncurry3 WallSurface
            , \s -> (wlFeature s, wlLod3Model s, wlOpenings s)
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
