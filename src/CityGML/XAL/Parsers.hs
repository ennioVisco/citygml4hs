module CityGML.XAL.Parsers where

import           CityGML.XAL.Types

import           Text.XML.HXT.Core

instance XmlPickler XalAddressDetails where
    xpickle = xpXalAddress

xpXalAddress :: PU XalAddressDetails
xpXalAddress
  = xpElem "xal:AddressDetails" $
    xpElem "xal:Country" $
    xpWrap  ( \ (cn, lt, ln, tt, tnb, tnm) ->
              XalAddressDetails cn lt ln tt tnb tnm
            , \ d ->    (   xCountryName        d
                        ,   xLocalityType       d
                        ,   xLocalityName       d
                        ,   xThoroughfareType   d
                        ,   xThoroughfareNumber d
                        ,   xThoroughfareName   d
                        )
            ) $
    xp6Tuple    (xpElem "xal:CountryName"               xpText)
                (xpElem "xal:Locality"      $
                        xpAttr "Type"                   xpText)
                (xpElem "xal:Locality"      $
                        xpElem "xal:LocalityName"       xpText)
                (xpElem "xal:Thoroughfare"  $
                                        xpAttr "Type"   xpText)
                (xpElem "xal:Thoroughfare"  $
                        xpElem "xal:ThoroughfareNumber" xpText)
                (xpElem "xal:Thoroughfare"  $
                        xpElem "xal:ThoroughfareName" xpText)
