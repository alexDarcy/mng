{-# LANGUAGE DeriveGeneric, 
             DeriveDataTypeable,
             DeriveFunctor, 
             TypeSynonymInstances,
             FlexibleInstances,
             OverloadedStrings,
             QuasiQuotes  #-}


module GenericEntries where

import qualified Data.Text as T
import GHC.Generics
import Data.Csv
import Data.Data
import Data.Typeable
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.List as L
import qualified Data.Vector as V
import Text.RawString.QQ

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

---

data EntityType = Movie | Comic
  deriving (Data, Typeable)

instance Show EntityType where
  show Movie = "movie"
  show Comic = "comic"


-- Exporting to HTML

showLine :: T.Text -> String
showLine str = "<td>" ++ T.unpack str ++ "</td>\n"


asTable x = "<tr>\n"
           ++ showLine ( title x )
           ++ showLine ( rating x )
           ++ showLine ( serie x )
           ++ showLine ( year x )
           ++ "</tr>\n"



serieAsHTML :: String -> [Entry] -> String
serieAsHTML title l = "<h2>" ++ title ++ "</h2>"
               ++ "<table>\n" 
               ++ intercalate "\n" (map asTable l)
               ++ "</table>\n"

serieAsHTMLTodo Comic = serieAsHTML "To read"
serieAsHTMLTodo _      = serieAsHTML "To watch"

serieTitle :: [Entry] -> String
serieTitle l 
  | title == "" = "Other"
  | otherwise   = title
    where title = T.unpack . serie . L.head $ l
  
serieAsHTMLOther l = serieAsHTML (serieTitle l) l

isTodo x = x `L.elem` map T.pack ["toread", "towatch"]

groupFunc :: EntityType -> Entry -> Entry  -> Bool
groupFunc Comic x y = L.all (isTodo . status) [x,y]
                     || serie x == serie y
groupFunc _ x y = status x == status y

htmlContent :: EntityType -> V.Vector Entry -> String
htmlContent etype vector = htmlHeader 
                             ++ htmlTitle etype
                             ++ htmlEntries etype (V.toList vector)
                             ++ htmlFooter

htmlEntries :: EntityType -> [Entry] -> String
htmlEntries c l = serieAsHTMLTodo c toRead
               ++ L.concatMap serieAsHTMLOther readGrouped
  where 
    (toRead, read) = L.partition (isTodo . status) l
    grouped = groupBy (groupFunc c) read
    readGrouped = sortBy (comparing serieTitle) grouped


htmlHeader = [r|
<!DOCTYPE html>
<meta charset="utf-8">
<link rel="stylesheet" type="text/css" href="css/style.css" />
<link rel="stylesheet" type="text/css" href="/assets/fonts/stylesheet.css" charset="utf-8"/>
<body>|]

--data Category = Comics | Movies | Anime deriving (Show)--, Data)

htmlTitle :: EntityType -> String
htmlTitle x =  "<h1>" ++ show x ++ "</h1>"

htmlFooter = "</body>"
