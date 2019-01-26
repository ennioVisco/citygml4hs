module CityGML.XAL.Parsers where

import           CityGML.XAL.Types

import           Text.XML.HXT.Core

instance XmlPickler XalAddressDetails where
    xpickle = xpXalAddress

xpXalAddress :: PU XalAddressDetails
xpXalAddress
  = xpElem "xAL:AddressDetails" $
    xpElem "xAL:Country" $
    xpWrap  ( uncurry XalAddressDetails
            , \ d -> (xCountryName d, xLocality d)
            ) $
    xpPair  (xpElem "xAL:CountryName"  xpText)
            xpLocality

xpLocality :: PU XalLocality
xpLocality
  = xpElem "xAL:Locality" $
    xpWrap      ( uncurry3 XalLocality
                , \ l -> (xLocalityType l, xLocalityName l, xThoroughfare l)
                ) $
    xpTriple    (xpAttr "Type"              xpText)
                (xpElem "xAL:LocalityName"  xpText)
                xpThoroughfare


xpThoroughfare :: PU XalThoroughfare
xpThoroughfare
  = xpElem "xAL:Thoroughfare"  $
    xpWrap      ( uncurry3 XalThoroughfare
                , \ l ->    ( xThoroughfareType   l
                            , xThoroughfareNumber l
                            , xThoroughfareName   l )
                ) $
    xpTriple    (xpAttr "Type"                   xpText)
                (xpElem "xAL:ThoroughfareNumber" xpText)
                (xpElem "xAL:ThoroughfareName"   xpText)
