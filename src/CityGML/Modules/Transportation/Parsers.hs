-- ------------------------------------------------------------

{- |
   Module     : CityGML.Modules.Transportation.Parsers

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Parsers (i.e. 'XMLPickler's) related to the Types defined in
   'CityGML.Modules.Transportation.Types'.

-}

-- ------------------------------------------------------------

module CityGML.Modules.Transportation.Parsers where

import           CityGML.Core.Parsers
import           CityGML.Core.Types
import           CityGML.GML.Parsers
import           CityGML.Modules.Transportation.Types
import           Text.XML.HXT.Core




instance XmlPickler TransportationObject where
    xpickle = xpTransportation

instance XmlPickler TransportationComplex where
    xpickle = xpTranComplex

instance XmlPickler TrafficArea where
    xpickle = xpTrafficArea

instance XmlPickler AuxiliaryTrafficArea where
    xpickle = xpAuxTrafficArea

instance XmlPickler TransportationData where
    xpickle = xpTranData

instance XmlPickler Road where
    xpickle = xpRoad

xpTransportation :: PU TransportationObject
xpTransportation = xpAlt tag ps
            where
            tag (TCRO _) = 0
            tag (TCRA _) = 1
            tag (TCSQ _) = 2
            tag (TCTR _) = 3
            tag (TA _)   = 4
            tag (AT _)   = 5
            ps = [  xpWrap  ( TCRO
                            , \ (TCRO c) -> c
                            ) $
                    xpRoad

                 ,  xpWrap  ( TCRA
                            , \ (TCRA c) -> c
                            ) $
                    xpRailway

                 ,  xpWrap  ( TCSQ
                            , \ (TCSQ c) -> c
                            ) $
                    xpSquare

                 ,  xpWrap  ( TCTR
                            , \ (TCTR c) -> c
                            ) $
                    xpTrack

                 ,  xpWrap  ( TA
                            , \ (TA a) -> a
                            ) $
                    xpTrafficArea

                 ,  xpWrap  ( AT
                            , \ (AT a) -> a
                            ) $
                    xpAuxTrafficArea
                 ]

xpTrafficArea :: PU TrafficArea
xpTrafficArea
  = xpElem  "tran:TrafficArea" $
    xpWrap  ( uncurry TrafficArea
            , \ a -> (taObject  a, taData a)
            ) $
    xpPair  xpCityObject
            xpTranData

xpAuxTrafficArea :: PU AuxiliaryTrafficArea
xpAuxTrafficArea
  = xpElem  "tran:AuxiliaryTrafficArea" $
    xpWrap  ( uncurry AuxiliaryTrafficArea
            , \ a -> (ataObject  a, ataData a)
            ) $
    xpPair  xpCityObject
            xpTranData

xpTranComplex :: PU TransportationComplex
xpTranComplex
  = xpWrap  ( \(o, l0, l1, d, ta, at ) -> TransportationComplex o l0 l1 d ta at
            , \ c -> ( tcObject c, tcLod0Network c, tcLod1Model c
                     , tcData c, tcTrafficArea c, tcAuxiliaryTrafficArea c )
            ) $
    xp6Tuple    xpCityObject
                (xpOption $ xpElem "tran:lod0Network"        xpGeometricComplex)
                (xpOption $ xpElem "tran:lod1MultiSurface"       xpMultiSurface)
                xpTranData
                (xpList   $ xpElem "tran:trafficArea"             xpTrafficArea)
                (xpList   $ xpElem "tran:auxiliaryTrafficArea" xpAuxTrafficArea)

xpTranData :: PU TransportationData
xpTranData
 =  xpWrap  ( \(c, f, u, sm, l2, l3, l4) -> TransportationData c f u sm l2 l3 l4
            , \ d -> (  tranClass d, tranFunction d, tranUsage d,
                        tranSurfaceMaterial d,
                        tranLod2Model d, tranLod3Model d, tranLod4Model d )
            ) $
    xp7Tuple    (xpOption $ xpElem "tran:class"                xpCodeType)
                (xpList   $ xpElem "tran:function"             xpCodeType)
                (xpList   $ xpElem "tran:usage"                xpCodeType)
                (xpOption $ xpElem "tran:surfaceMaterial"      xpCodeType)
                (xpOption $ xpElem "tran:lod2MultiSurface" xpMultiSurface)
                (xpOption $ xpElem "tran:lod3MultiSurface" xpMultiSurface)
                (xpOption $ xpElem "tran:lod4MultiSurface" xpMultiSurface)

xpRoad :: PU Road
xpRoad
  = xpElem "tran:Road"    $
    xpWrap  ( Road
            , \ (Road tc) -> tc
            ) $
    xpTranComplex

xpRailway :: PU Railway
xpRailway
  = xpElem "tran:Railway"    $
    xpWrap  ( Railway
            , \ (Railway tc) -> tc
            ) $
    xpTranComplex

xpSquare :: PU Square
xpSquare
  = xpElem "tran:Square"    $
    xpWrap  ( Square
            , \ (Square tc) -> tc
            ) $
    xpTranComplex

xpTrack :: PU Track
xpTrack
  = xpElem "tran:Track"    $
    xpWrap  ( Track
            , \ (Track tc) -> tc
            ) $
    xpTranComplex
