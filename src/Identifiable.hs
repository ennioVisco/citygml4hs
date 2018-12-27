{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Identifiable
    ( Identifiable
    , uid
    )
where

import           GHC.Generics

instance Identifiable a => Identifiable (Maybe a)

instance Identifiable a => Identifiable [a] where
    uid []     = ""
    uid (x:xs) = uid x ++ uid xs

instance Identifiable Char where
    uid = (:[])

instance Identifiable Int where
    uid = show

instance Identifiable Float where
    uid = show


class Identifiable a where
  uid :: a -> String

  default uid :: (Generic a, GIdentifiable (Rep a)) => a -> String
  uid = getid . from



class GIdentifiable t where
    getid :: t a -> String

instance GIdentifiable U1 where
    getid U1 = "UNIDENTIFIABLE_CITYGML_OBJ"

instance (GIdentifiable a, GIdentifiable b) => GIdentifiable (a :*: b) where
    getid (x :*: _) = getid x

instance (GIdentifiable a, GIdentifiable b) => GIdentifiable (a :+: b) where
  getid (L1 x) = getid x
  getid (R1 x) = getid x

instance (GIdentifiable a) => GIdentifiable (M1 i c a) where
  getid (M1 x) = getid x

instance (Identifiable a) => GIdentifiable (K1 i a) where
  getid (K1 x) = uid x
