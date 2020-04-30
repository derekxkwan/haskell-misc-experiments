{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Monad
import Data.Typeable
import qualified Data.ByteString.Char8 as C8

inFile :: FilePath
inFile = "/home/dxk/hsstuff/temp.html"


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
  mapM_ (print . join) $ allMovieTitles cur

