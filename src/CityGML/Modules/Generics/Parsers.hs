-- ------------------------------------------------------------

{- |
   Module     : CityGML.Modules.Generics.Parsers

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Parsers (i.e. 'XMLPickler's) related to the Types defined in
   'CityGML.Modules.Generics.Types'.

-}

-- ------------------------------------------------------------

module CityGML.Modules.Generics.Parsers where

import           CityGML.GML.Parsers
import           CityGML.Modules.Generics.Types
import           Text.XML.HXT.Core


instance XmlPickler GenLod1Model where
    xpickle = xpGenLod1Model

instance XmlPickler GenericCityObject where
    xpickle = xpGenerics

instance XmlPickler GenericAttribute where
    xpickle = xpGenericAttribute

xpGenLod1Model :: PU GenLod1Model
xpGenLod1Model
  = xpAlt tag ps
        where
        tag (GenLod1Geometry _) = 0
        ps = [  xpWrap  ( GenLod1Geometry
                        , \ (GenLod1Geometry s) -> s
                        ) $
                xpElem "gen:lod1Geometry" xpMultiSurface
             ]

xpGenerics :: PU GenericCityObject
xpGenerics =
    xpElem "gen:GenericCityObject"    $
    xpWrap  ( \ (f,l1) -> GenericCityObject f l1
            , \ g ->    ( genFeature g , genLod1Model g )
            ) $
    xpPair      xpFeature
                xpGenLod1Model

xpGenericAttribute :: PU GenericAttribute
xpGenericAttribute
  = xpAlt tag ps
        where
        tag (StringAttribute _ _) = 0
        tag (IntAttribute    _ _) = 1
        tag (DoubleAttribute _ _) = 2
        ps = [  xpElem  "gen:stringAttribute" $
                xpWrap  ( uncurry StringAttribute
                        , \ a -> (gsaName a, gsaValue a)
                        ) $
                xpPair  (xpAttr "name"      xpText)
                        (xpElem "gen:value" xpText)

             ,  xpElem  "gen:intAttribute" $
                xpWrap  ( uncurry IntAttribute
                        , \ a -> (giaName a, giaValue a)
                        ) $
                xpPair  (xpAttr "name"      xpText)
                        (xpElem "gen:value" xpText)

            ,  xpElem   "gen:doubleAttribute" $
                        xpWrap  ( uncurry DoubleAttribute
                                , \ a -> (gdaName a, gdaValue a)
                                ) $
                        xpPair  (xpAttr "name"      xpText)
                                (xpElem "gen:value" xpText)
             ]
