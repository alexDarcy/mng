{-# LANGUAGE DeriveGeneric, DeriveFunctor, 
             TypeSynonymInstances,
             FlexibleInstances #-}


module Movies where

import qualified Data.Text as T
import GHC.Generics
import Data.Csv
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.List as L

-- Ordering is done on the order of the fields

data GenericEntry a = Entry { title :: a
                   , serie :: a
                   , rating :: a
                   , status :: a
                   , year :: a
                   }
    deriving (Generic, Show, Functor)

-- We use a functor to iterate through the records
type Entry = GenericEntry T.Text

-- Sort by status first
instance Ord Entry where
    compare = comparing status <> comparing serie <> comparing title

-- Eq must be defined for Ord
instance Eq Entry where
    x == y = L.and [ f x == f y 
                           | f <- [status, serie, title, year] ]
instance FromNamedRecord Entry
instance ToNamedRecord Entry

