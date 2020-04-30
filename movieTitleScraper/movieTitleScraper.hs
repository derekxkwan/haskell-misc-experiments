{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Data.Typeable
import Data.Maybe

inFile :: FilePath
inFile = "temp.html"

outFile :: FilePath
outFile = "movieTitles.txt"

movieTitles :: Scraper String [[String]]
movieTitles = chroots "tr" $ chroots "td" $  do
  idx <- position
  guard $ idx == 0
  content <- text "a"
  return content
  


allMovieTitles :: String -> Maybe [[String]]
allMovieTitles inStr = scrapeStringLike inStr movieTitles

main :: IO ()
main = do
  cur <- readFile inFile
  writeFile outFile $ catMaybes $ mapM (unlines . join) $ allMovieTitles cur

