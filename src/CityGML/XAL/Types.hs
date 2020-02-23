{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.XAL.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Types related to the XAL specification subset used by CityGML.

-}

-- ------------------------------------------------------------

module CityGML.XAL.Types where

import           GHC.Generics
import           Data.Data
import           Data.Binary
import           Identifiable


{-
<xal:AddressDetails>
  <xal:Country>
    <xal:CountryName>Germany</xal:CountryName>
    <xal:Locality Type="Town">
      <xal:LocalityName>Solingen</xal:LocalityName>
      <xal:Thoroughfare Type="Street">
        <xal:ThoroughfareNumber>34</xal:ThoroughfareNumber>
        <xal:ThoroughfareName>Müngstener Straße</xal:ThoroughfareName>
      </xal:Thoroughfare>
    </xal:Locality>
  </xal:Country>
</xal:AddressDetails>
-}
data XalAddressDetails = XalAddressDetails
    {   xCountryName :: String
    ,   xLocality    :: XalLocality
    } deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data XalLocality = XalLocality
    {   xLocalityType :: String
    ,   xLocalityName :: String
    ,   xThoroughfare :: XalThoroughfare
    } deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

data XalThoroughfare = XalThoroughfare
    {   xThoroughfareType   :: String
    ,   xThoroughfareNumber :: String
    ,   xThoroughfareName   :: String
    } deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)
