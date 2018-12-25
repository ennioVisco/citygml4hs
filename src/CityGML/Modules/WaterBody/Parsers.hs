-- ------------------------------------------------------------

{- |
   Module     : CityGML.Modules.WaterBody.Parsers

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   Parsers (i.e. 'XMLPickler's) related to the Types defined in
   'CityGML.Modules.WaterBody.Types'.

-}

-- ------------------------------------------------------------

module CityGML.Modules.WaterBody.Parsers where

import           CityGML.GML.Parsers
import           CityGML.Modules.WaterBody.Types
import           Text.XML.HXT.Core


instance XmlPickler WtrLod1Model where
    xpickle = xpAlt tag ps
        where
        tag (WtrLod1MultiSurf _) = 0
        ps = [  xpWrap  ( WtrLod1MultiSurf
                        , \ (WtrLod1MultiSurf s) -> s
                        ) $
                xpElem "wtr:lod1MultiSurface" xpMultiSurface
             ]

instance XmlPickler WaterObject where
    xpickle = xpWaterBody

xpWaterBody :: PU WaterObject
xpWaterBody =
    xpElem "wtr:WaterBody"    $
    xpWrap  (\(f,l1) -> WaterBody f l1
            , \ w ->    ( wtrFeature w
                        , wtrLod1Model w
                        )
            ) $
    xpPair      xpFeature
                (xpOption xpickle)
