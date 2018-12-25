
-- ------------------------------------------------------------

{- |
   Module     : CityGML.XLink.Parsers

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   This modules handles the concrete implementations of xlink-related
   picklers.

-}

-- ------------------------------------------------------------

module CityGML.XLink.Parsers where

import           CityGML.XLink.Types
import           Text.XML.HXT.Core

instance XmlPickler XLink where
    xpickle = xpXLink

xpXLink :: PU XLink
xpXLink
  = xpWrap  ( XLink
            , \ (XLink url) -> url
            )
    (xpAttr "xlink:href" xpText)
