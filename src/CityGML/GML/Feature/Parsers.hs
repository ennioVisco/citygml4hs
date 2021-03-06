-- ------------------------------------------------------------

{- |
   Module     : CityGML.GML.Feature.Parsers

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   This module provides the picklers implementation for the _Feature
   class of the GML specification (actually focusing on the CityGML subset of
   features).
   For more information, see 'CityGML.GML.Feature.Types'

-}

-- ------------------------------------------------------------

module CityGML.GML.Feature.Parsers where

import           CityGML.GML.Base
import           CityGML.GML.Feature.Types
import           Text.XML.HXT.Core

instance XmlPickler Feature where
    xpickle = xpFeature

instance XmlPickler BoundedBy where
    xpickle = xpBoundedBy

instance XmlPickler FeatureCollection where
    xpickle = xpFeatureCollection

-- | Prickler for the base _Feature abstract element.
-- See 7.4.1.2 of GML Specification.
xpFeature :: PU Feature
xpFeature
  = xpWrap  ( uncurry Feature
            , \ f -> (fGml f, boundedBy f)
            ) $

    xpPair  xpGML
            (xpOption xpBoundedBy)

-- | Prickler for the gml:BoundedBy GML property.
-- See 7.4.1.3 of GML Specification.
xpBoundedBy :: PU BoundedBy
xpBoundedBy
  = xpElem  "gml:boundedBy" $
    xpElem  "gml:Envelope"  $
    xpWrap      (\(s,l,u) -> BoundedBy s l u
                , \ b -> ( bSrsReference b, lCorner b, uCorner b )
                ) $

    xpTriple    xpSRSReferenceGroup
                (xpElem "gml:lowerCorner"   xpText)
                (xpElem "gml:upperCorner"   xpText)

-- | Prickler for the _FeatureCollection GML element.
-- See 7.4.1.8 of GML Specification.
xpFeatureCollection :: PU FeatureCollection
xpFeatureCollection
  = xpWrap  ( uncurry FeatureCollection
            , \ f -> (feature f, featureMember f)
            ) $

    xpPair  xpFeature
            (xpList $ xpElem "gml:featureMember" xpFeature)
