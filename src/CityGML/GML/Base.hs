{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.GML.Base

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   This module contains the foundations of all GML types

-}

-- ------------------------------------------------------------

module CityGML.GML.Base where

import           Data.Binary
import           Data.Data
import           Data.Maybe
import           GHC.Generics
import           Identifiable
import           Text.XML.HXT.Core

-- | 'GML' provides the most basic interface to the underlying GML data.
--  TODO: metaDataProperty maybe needed? see @7.2.2.8 of the GML specification
data GML = GML
    {   gid         :: Maybe String  -- ^ TODO: should be changed to a ID type
    ,   name        :: [CodeType]    -- ^ List of domain-specific names.
    ,   description :: Maybe String  -- ^ Optional description of the element.
    -- metaDataProperty
    } deriving (Read, Show, Eq, Data, Generic, Binary)

{- | As stated in the GML specification:
    This is a generalized type to be used for a term, keyword or name. It adds a
    XML attribute codeSpace to a term, where the value of the (optional)
    codeSpace should indicate a dictionary, thesaurus, classification scheme,
    authority, or pattern for the term'CodeType' refers to.

    Note that
    in all cases the rules for the values, including such things as uniqueness
    constraints, are set by the authority responsible for the codeSpace.
-}
data CodeType = CodeType
    {   value     :: String         -- ^ The actual name of the element.
    ,   codeSpace :: Maybe String   -- ^ The semantical space to which the name refers.
    } deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

{- | As stated in the GML 3.2.1+ specification (§ 10.1.3.2):
    The attribute group gml:SRSReferenceGroup is an optional reference to the
    CRS used by this geometry, with optional additional information to simplify
    the processing of the coordinates when a more complete definition of the
    CRS is not needed.

    Note: attributeGroup "gml:SRSInformationGroup" not implemented yet!
-}
data SRSReferenceGroup = SRSReferenceGroup
    {   srsName      :: String
    ,   srsDimension :: Maybe Int
    } deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)

instance XmlPickler GML where
    xpickle = xpGML

instance XmlPickler CodeType where
    xpickle = xpCodeType

instance XmlPickler SRSReferenceGroup where
    xpickle = xpSRSReferenceGroup


-- | Prickler for the base _GML abstract element.
-- See 7.2.2.2 of GML Specification.
xpGML :: PU GML
xpGML = xpWrap      ( uncurry3 GML
                    , \ g -> (gid g, name g, description g)
                    ) $

        xpTriple    (xpOption $ xpAttr "gml:id"             xpText      )
                    (xpList   $ xpElem "gml:name"           xpCodeType  )
                    (xpOption $ xpElem "gml:description"    xpText      )


-- | Prickler for the CodeType abstract property, i.e. gml:codeType.
-- See 7.3.2.5 of GML Specification.
xpCodeType :: PU CodeType
xpCodeType =    xpWrap  ( uncurry CodeType
                        , \ t -> (value t, codeSpace t)
                        ) $

                xpPair  xpText
                        (xpOption $ xpAttr "codeSpace" xpText)


-- | Prickler for the SRSReferenceGroup property group, i.e. srsName,
--  srsDimension etc.
-- See § 10.1.3.2 of GML Specification.
xpSRSReferenceGroup :: PU SRSReferenceGroup
xpSRSReferenceGroup =   xpWrap  ( uncurry SRSReferenceGroup
                                , \ t -> (srsName t, srsDimension t)
                                ) $

                        xpPair  (xpAttr            "srsName"      xpText)
                                (xpOption $ xpAttr "srsDimension" xpPrim)

-- | The Identifiable class is the foundational helper to extract a UniqueID
-- from a GML instance.
instance Identifiable GML where
    uid (GML Nothing [] Nothing)          = "UNKNOWN_ID"
    uid (GML (Just i) (n:_) _)            = i ++ extendID n
    uid (GML (Just i) _ _)                = i
    uid (GML _ (n:_) _) | isNothing
                            (codeSpace n) = value n
                        | otherwise       = fromJust (codeSpace n) ++ value n
    uid (GML _ _ (Just d))                = d

extendID :: CodeType -> String
extendID (CodeType v s) | isNothing s = "::" ++ v
                        | otherwise   = "::" ++ fromJust s ++ v
