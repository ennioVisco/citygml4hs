
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
  = xpWrap  ( \ (f, cd, td, e, g, rt, rw) ->
              CityObject f cd td e g rt rw
            , \ o ->    (   oFeature            o
                        ,   oCreationDate       o
                        ,   oTerminationDate    o
                        ,   oExternalReference  o
                        ,   oGeneralizesTo      o
                        ,   oRelativeToTerrain  o
                        ,   oRelativeToWater    o
                        )
            ) $
    xp7Tuple    xpFeature
                (xpOption $ xpElem "core:creationDate"            xpText)
                (xpOption $ xpElem "core:terminationDate"         xpText)
                (xpList   $ xpElem "core:externalReference"     xpExtRef)
                (xpList   $ xpElem "core:generalizesTo"     xpCityObject)
                (xpOption $ xpElem "core:relativeToTerrain"       xpText)
                (xpOption $ xpElem "core:relativeToWater"         xpText)

xpExtRef :: PU ExternalReference
xpExtRef
  = xpWrap  ( uncurry ExternalReference
            , \ r -> (erInformationSystem r, erExternalObjRef r)
            ) $
    xpPair  (xpOption $ xpElem "core:informationSystem" xpText)
            (xpElem            "core:externalObject"  xpExtObj)

xpExtObj :: PU ExternalObject
xpExtObj
  = xpWrap  ( uncurry ExternalObject
            , \ o -> (eoName o, eoUri o)
            ) $
    xpPair  (xpElem            "core:name" xpText)
            (xpOption $ xpElem "core:uri"  xpText)

xpAddress :: PU Address
xpAddress
  = xpElem "core:Address"    $
    xpElem "core:xalAddress" $
    xpWrap  ( Address
            , \ (Address a) -> a
            )
    xpXalAddress
