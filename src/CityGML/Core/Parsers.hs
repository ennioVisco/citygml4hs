
-- ------------------------------------------------------------

{- |
   Module     : CityGML.Core.Parsers

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Parsers (i.e. 'XMLPickler's) related to the Types defined in
   'CityGML.Core.Types'.

-}

-- ------------------------------------------------------------

module CityGML.Core.Parsers where

import           Text.XML.HXT.Core

import           CityGML.Core.Types
import           CityGML.GML.Parsers
import           CityGML.Modules.Generics.Parsers (xpGenericAttribute)
import           CityGML.XAL.Parsers

instance XmlPickler CityObject where
    xpickle = xpCityObject

instance XmlPickler ExternalReference where
    xpickle = xpExtRef

instance XmlPickler ExternalObject where
    xpickle = xpExtObj

instance XmlPickler Address where
    xpickle = xpAddress


xpCityObject :: PU CityObject
xpCityObject
  = xpWrap  ( \ (f, cd, td, e, g, rt, rw, ga) ->
              CityObject f cd td e g rt rw ga
            , \ o ->    (   oFeature            o
                        ,   oCreationDate       o
                        ,   oTerminationDate    o
                        ,   oExternalReference  o
                        ,   oGeneralizesTo      o
                        ,   oRelativeToTerrain  o
                        ,   oRelativeToWater    o
                        ,   oAttributes         o
                        )
            ) $
    xp8Tuple    xpFeature
                (xpOption $ xpElem "creationDate"            xpText)
                (xpOption $ xpElem "terminationDate"         xpText)
                (xpList   $ xpElem "externalReference"     xpExtRef)
                (xpList   $ xpElem "generalizesTo"     xpCityObject)
                (xpOption $ xpElem "relativeToTerrain"       xpText)
                (xpOption $ xpElem "relativeToWater"         xpText)
                (xpList   xpGenericAttribute)

xpExtRef :: PU ExternalReference
xpExtRef
  = xpWrap  ( uncurry ExternalReference
            , \ r -> (erInformationSystem r, erExternalObjRef r)
            ) $
    xpPair  (xpOption $ xpElem "informationSystem" xpText)
            (xpElem            "externalObject"  xpExtObj)

xpExtObj :: PU ExternalObject
xpExtObj
  = xpWrap  ( uncurry ExternalObject
            , \ o -> (eoName o, eoUri o)
            ) $
    xpPair  (xpElem            "name" xpText)
            (xpOption $ xpElem "uri"  xpText)

xpAddress :: PU Address
xpAddress
  = xpElem "Address"    $
    xpElem "xalAddress" $
    xpWrap  ( Address
            , \ (Address a) -> a
            )
    xpXalAddress
