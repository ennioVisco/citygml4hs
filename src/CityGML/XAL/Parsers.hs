module CityGML.XAL.Parsers where

import           CityGML.XAL.Types

import           Text.XML.HXT.Core

instance XmlPickler XalAddressDetails where
    xpickle = xpXalAddress

xpXalAddress :: PU XalAddressDetails
xpXalAddress
  = xpElem "xal:AddressDetails" $
    xpElem "xal:Country" $
    xpWrap  ( uncurry XalAddressDetails
            , \ d -> (xCountryName d, xLocality d)
            ) $
    xpPair  (xpElem "xal:CountryName"  xpText)
            xpLocality

xpLocality :: PU XalLocality
xpLocality
  = xpElem "xal:Locality" $
    xpWrap      ( uncurry3 XalLocality
                , \ l -> (xLocalityType l, xLocalityName l, xThoroughfare l)
                ) $
    xpTriple    (xpAttr "Type"              xpText)
                (xpElem "xal:LocalityName"  xpText)
                xpThoroughfare


xpThoroughfare :: PU XalThoroughfare
xpThoroughfare
  = xpElem "xal:Thoroughfare"  $
    xpWrap      ( uncurry3 XalThoroughfare
                , \ l ->    ( xThoroughfareType   l
                            , xThoroughfareNumber l
                            , xThoroughfareName   l )
                ) $
    xpTriple    (xpAttr "Type"                   xpText)
                (xpElem "xal:ThoroughfareNumber" xpText)
                (xpElem "xal:ThoroughfareName"   xpText)
