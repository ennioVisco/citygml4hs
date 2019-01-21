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

import           CityGML.Core.Parsers
import           CityGML.GML.Parsers
import           CityGML.Modules.Building.Types
import           Text.XML.HXT.Core


instance XmlPickler BuildingModels where
    xpickle = xpBldgModels

instance XmlPickler BuildingData where
    xpickle = xpBuildingData

instance XmlPickler BldgLod1Int where
    xpickle = xpBldgLod1Int

instance XmlPickler BldgLod2Int where
    xpickle = xpBldgLod2Int

instance XmlPickler BldgLod0Model where
    xpickle = xpBldgLod0

instance XmlPickler BldgLod1Model where
    xpickle = xpBldgLod1

instance XmlPickler BldgLod2Model where
    xpickle = xpBldgLod2

instance XmlPickler BldgLod3Model where
    xpickle = xpBldgLod3

instance XmlPickler BuildingInfo where
    xpickle = xpBldgInfo

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

xpBldgLod1Int :: PU BldgLod1Int
xpBldgLod1Int
    = xpAlt tag ps
        where
        tag (BldgLod1Int _) = 0
        ps = [  xpWrap  ( BldgLod1Int
                        , \ (BldgLod1Int s) -> s
                        ) $
                xpElem "bldg:lod1TerrainIntersection" xpMultiCurve
             ]

xpBldgLod2Int :: PU BldgLod2Int
xpBldgLod2Int
    = xpAlt tag ps
        where
        tag (BldgLod2Int _) = 0
        ps = [  xpWrap  ( BldgLod2Int
                        , \ (BldgLod2Int s) -> s
                        ) $
                xpElem "bldg:lod2TerrainIntersection" xpMultiCurve
             ]

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
        tag (BldgLod2Multi _) = 1
        ps = [  xpWrap  ( BldgLod2Solid
                        , \ (BldgLod2Solid s) -> s
                        ) $
                xpElem "bldg:lod2Solid" xpSolid

            ,   xpWrap  ( BldgLod2Multi
                        , \ (BldgLod2Multi s) -> s
                        ) $
                xpElem "bldg:lod2MultiSurface" xpMultiSurface
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
xpBuilding
    = xpAlt tag ps
        where
        tag (Building _)     = 0
        tag (BuildingPart _) = 1
        ps = [  xpWrap  ( Building
                        , \ (Building b) -> b
                        ) $
                xpElem  "bldg:Building" xpBuildingData
            ,   xpWrap  ( BuildingPart
                        , \ (BuildingPart b) -> b
                        ) $
                xpElem "bldg:BuildingPart" xpBuildingData
             ]



xpBuildingData :: PU BuildingData
xpBuildingData =
    xpWrap  (\(g, bi, bm, bt, b,i, ps, a) ->
                BldgData g bi bm bt b i ps a
            , \ b ->    (   bObject        b
                        -- Building Optional Information
                        ,   bInfo          b
                        -- Building Models
                        ,   bModels        b
                        -- Building Intersections
                        ,   bIntersection  b
                        -- Building External Interfaces
                        ,   bInstallations b
                        ,   bBoundedBy     b
                        ,   bBuildingParts b
                        ,   bAddress       b
                        )
            ) $
    xp8Tuple    xpCityObject
                -- Building Optional Information
                xpBldgInfo
                -- Building Models
                xpBldgModels
                -- Building Intersections
                xpBldgInt
                -- Building External Interfaces
                (xpList   $ xpElem "bldg:outerBuildingInstallation" xpBldgInst)
                (xpList   $ xpElem "bldg:boundedBy"             xpBldgBoundary)
                (xpList   $ xpElem "bldg:consistsOfBuildingPart"    xpBuilding)
                (xpOption $ xpElem "bldg:address"                    xpAddress)

xpBldgInt :: PU BuildingIntersections
xpBldgInt
  = xpWrap  ( uncurry BuildingIntersections
        , \ b ->    (   bLod1TerrainInt b
                    ,   bLod2TerrainInt b
                    )
        ) $
    xpPair  (xpOption xpBldgLod1Int)
            (xpOption xpBldgLod2Int)

xpBldgInfo :: PU BuildingInfo
xpBldgInfo
  = xpWrap  (\(f,r,h,y,s) ->
            BuildingInfo f r h y s
        , \ b ->    (   bFunction      b
                    ,   bRoofType      b
                    ,   bHeight        b
                    ,   bYearOfConstr  b
                    ,   bStAboveGround b
                    )
        ) $
    xp5Tuple    (xpOption $ xpElem "bldg:function"           xpText)
                (xpOption $ xpElem "bldg:roofType"           xpText)
                (xpOption                                 xpMeasure)
                (xpOption $ xpElem "bldg:yearOfConstruction" xpText)
                (xpOption $ xpElem "bldg:storeysAboveGround" xpPrim)

xpBldgModels :: PU BuildingModels
xpBldgModels
  = xpWrap  (\(l0f,l0r,l1,l2,l3) ->
            BuildingModels l0f l0r l1 l2 l3
        , \ b ->    (   bLod0FootPrint b
                    ,   bLod0RoofEdge  b
                    ,   bLod1Solid     b
                    ,   bLod2Solid     b
                    ,   bLod3Solid     b
                    )
        ) $
    xp5Tuple    (xpOption xpBldgLod0)
                (xpOption xpBldgLod0)
                (xpOption xpBldgLod1)
                (xpOption xpBldgLod2)
                (xpOption xpBldgLod3)

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
 =  xpWrap  ( uncurry4 WallSurface
            , \s -> (wlObject s, wlLod2Model s, wlLod3Model s, wlOpenings s)
            ) $
    xp4Tuple    xpCityObject
                (xpOption xpBldgLod2)
                (xpOption xpBldgLod3)
                (xpList $ xpElem "bldg:opening" xpOpening)

xpRoofSurface :: PU RoofSurface
xpRoofSurface
 =  xpWrap  ( uncurry4 RoofSurface
            , \s -> (rfObject s, rfLod2Model s, rfLod3Model s, rfOpenings s)
            ) $
    xp4Tuple    xpCityObject
                (xpOption xpBldgLod2)
                (xpOption xpBldgLod3)
                (xpList $ xpElem "bldg:opening" xpOpening)

xpBuildingSurface :: PU BuildingSurface
xpBuildingSurface
  =  xpWrap     ( uncurry3 BuildingSurface
                , \s -> (bsObject s, bsLod2Model s, bsLod3Model s)) $

    xpTriple    xpCityObject
                (xpOption xpBldgLod2)
                (xpOption xpBldgLod3)

xpOpening :: PU Opening
xpOpening
 =  xpAlt tag ps
       where
       tag (Door   _ _) = 0
       tag (Window _ _) = 1
       ps = [   xpElem  "bldg:Door" $
                xpWrap  ( uncurry Door
                        , \d -> (dObject d, dLod3Model d)
                        ) $
                xpPair  xpCityObject
                        xpBldgLod3
           ,    xpElem  "bldg:Window" $
                xpWrap  ( uncurry Window
                        , \w -> (wObject w, wLod3Model w)
                        ) $
                xpPair  xpCityObject
                        xpBldgLod3
          ]


xpMeasure :: PU Measure
xpMeasure
  = xpElem "bldg:measuredHeight" $
    xpWrap  ( uncurry Height
            , \m -> (mUom m, mValue m)) $
    xpPair  (xpAttr "uom" xpText)
            xpPrim
