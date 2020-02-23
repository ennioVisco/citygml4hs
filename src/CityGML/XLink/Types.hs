{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- ------------------------------------------------------------

{- |
   Module     : CityGML.XLink.Types

   Maintainer : Ennio Visconti (ennio.visconti@mail.polimi.it)
   Stability  : stable
   Portability: portable

   This module handles the definition of Xlinks

-}

-- ------------------------------------------------------------

module CityGML.XLink.Types where

import           GHC.Generics
import           Data.Data
import           Data.Binary
import           Identifiable


newtype XLink = XLink String
    deriving (Read, Show, Eq, Data, Generic, Binary, Identifiable)
