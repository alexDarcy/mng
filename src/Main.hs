{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import Data.Csv
import Data.List (sort, insert)
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

data MovieArgs  = AddEntry 
                 { eType :: EntityType
                 , eSerie :: String
                 , eTitle :: String
                 , eRating :: String
                 , eStatus :: String
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


--type MovieArgs = GenericArgs String

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
list = ListEntries {}
        &= help "List all entries"
        &= name "list"

-- We do not need to repeat eType
export = ExportEntries { out = "html" &= typ "DIR" }
        &= help "Export to HTML"
        &= name "export"


insertEntry :: Entry -> [Entry] -> Maybe [Entry]
insertEntry e l  
  | e `elem` l = Nothing
  | otherwise    = Just $ insert e l

process :: MovieArgs -> (Header, V.Vector Entry) -> IO()
process (AddEntry etype s t y r st) (header, entries) = do
  let entry' = fmap T.pack (Entry t s y r st)
  let entries' = sort . V.toList $ entries
  let newEntries = insertEntry entry' entries'
  when (isNothing newEntries) $ error "Entry already in list"
  let newEntries' = encodeByName header (fromJust newEntries)
  file <- getFilePath etype 
  B.writeFile file newEntries'
  putStrLn "Entry added"
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
 
getOpts = cmdArgs $ modes [add , list, export ]
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
