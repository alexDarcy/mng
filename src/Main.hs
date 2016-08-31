{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import Data.Csv
import Data.List (sort, insert, elemIndex)
import Data.Maybe 
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Console.CmdArgs
import System.Directory 
import System.Environment (getArgs, withArgs, getEnv)
import System.FilePath.Posix ((</>))
import GHC.Generics
import GenericEntries 
import Paths_mng
import Debug.Trace

data MovieArgs  = AddEntry 
                 { eType :: EntityType
                 , eSerie :: String
                 , eTitle :: String
                 , eRating :: String
                 , eStatus :: String
                 , eYear :: String
                 }
                 | RemoveEntry
                 { eType :: EntityType
                 , eSerie :: String
                 , eTitle :: String
                 , eRating :: String
                 , eYear :: String
                 }
                 | ListEntries 
                 { eType :: EntityType }
                 | ExportEntries
                 { eType :: EntityType
                 , out :: String 
                 }
  deriving (Show, Data, Typeable)

instance Default EntityType where
  def = Movie

add = AddEntry 
      { eType = def  &= typ "[movie|comic]" &= argPos 0
      , eTitle = def  &= typ "title" &= argPos 1
      , eStatus = def  &= typ "status" &= argPos 2
      , eRating = def  &= typ "rating" &= explicit &= name "rating" &= name "r"
      , eYear = def  &= typ "year" &= explicit &= name "year" &= name "y"
      , eSerie = def  &= typ "serie" &= explicit &= name "serie" &= name "s"
      } 
        &= help "Adding an entry"
        &= name "add"

-- We do not need to repeat eType
remove = RemoveEntry {}
        &= help "Removing an entry"
        &= name "remove"

-- We do not need to repeat eType
list = ListEntries {}
        &= help "List all entries"
        &= name "list"

-- We do not need to repeat eType
export = ExportEntries { out = "html" &= typ "DIR" }
        &= help "Export to HTML"
        &= name "export"

-- Return the index of the entry with the same title if it exists
indexPrevious :: Entry -> [Entry] -> Maybe Int
indexPrevious e l 
  | not (null match) = elemIndex (head match) l
  | otherwise = Nothing
  where match = filter (\x -> title e == title x) l

-- Insert new element or update previous element with the same title
insertEntry :: Entry -> [Entry] -> (Maybe [Entry], String)
insertEntry e l  
  | e `elem` l = (Nothing, "Entry already exists")
  | isJust i = (Just l', "Entry updated")
  | otherwise = (Just $ insert e l, "Entry added")
  where 
    i = indexPrevious e l
    (l1, l2) = splitAt (fromJust i) l
    l' = l1 ++ [e] ++ tail l2

-- Remove element by title
removeEntry :: Entry -> [Entry] -> (Maybe [Entry], String)
removeEntry e l  
  | isJust i = (Just l', "Entry removed")
  | otherwise = (Just $ insert e l, "Entry added")
  where 
    i = indexPrevious e l
    (l1, l2) = splitAt (fromJust i) l
    l' = l1 ++ tail l2


sortAll = sort . V.toList 

writeAll (newEntries, mesg) etype header = do
  when (isNothing newEntries) $ error mesg
  putStrLn mesg
  let newEntries' = encodeByName header (fromJust newEntries)
  file <- getFilePath etype 
  B.writeFile file newEntries'

updateEntries f entry entries etype header = do
  let entry' = fmap T.pack entry
  let (newEntries, mesg) = f entry' (sortAll entries)
  writeAll (newEntries, mesg) etype header

process :: MovieArgs -> (Header, V.Vector Entry) -> IO()
process (AddEntry etype s t y r st) (header, entries) = do
  let entry = Entry t s y r st
  updateEntries insertEntry entry entries etype header
process (RemoveEntry etype s t y r) (header, entries) = do
  let entry = Entry t s y r ""
  updateEntries removeEntry entry entries etype header
process (ListEntries _) (_, vector) = V.mapM_ print vector
process (ExportEntries etype dir ) (_, vector) = createHTML etype dir vector

copyCSS :: String -> FilePath -> IO()
copyCSS cssFile dir = do
  src <- getDataFileName $ "css" </> cssFile
  let dir' = dir </> "css"
  createDirectoryIfMissing False dir'
  copyFile src (dir' </> cssFile)

createHTML :: EntityType -> String -> V.Vector Entry -> IO()
createHTML etype dir vector  = do
  let file = show etype ++ "s.html"
  createDirectoryIfMissing False dir
  writeFile (dir </> file) (htmlContent etype vector)
  mapM_ (`copyCSS` dir) ["style.css"]
 
getOpts = cmdArgs $ modes [add , remove, list, export ]
  &= summary "Manage movies, comics on the command-line."
  &= program "mng"

getFilePath :: EntityType  -> IO FilePath
getFilePath etype = do
  home <- getEnv "HOME"
  let dir = home </> ".mng"
  createDirectoryIfMissing False dir
  return $ dir </> (show etype ++ "s.csv")

defaultHeader = V.fromList $ map C.pack ["title", "serie", "rating", "status", "year"]

readData file = do
  input <- B.readFile file
  return $ case decodeByName input :: Either String (Header, V.Vector Entry) of
                  Left a -> error a
                  Right b -> b

getData file = do
  exist <- doesFileExist file
  if exist then readData file else return (defaultHeader, V.fromList [])

main = do
  -- Print help message when no argument
  args <- getArgs
  mode <- (if null args then withArgs ["--help"] else id) getOpts
  getFilePath (eType mode) >>= getData >>= process mode
