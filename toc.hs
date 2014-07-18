{-# LANGUAGE OverloadedStrings #-}

-- Generate the table of contents of the tutorial
import Data.Monoid
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

printTOC :: FilePath -> IO ()
printTOC mdfile = do
  content <- fmap T.lines $ T.readFile mdfile
  let titles = filter ("#" `T.isPrefixOf`) content
  mapM_ (T.putStrLn . transform) titles

transform :: T.Text -> T.Text
transform title =
  case T.breakOn " " title of
    (hashes, text) -> T.concat [hashToSpaces hashes, textToAnchor $ T.tail text]

  where hashToSpaces hashes = T.replicate (T.length hashes - 1) "    "
                           <> "- "
        textToAnchor t = 
          let anchor = T.map (\c -> if c == ' ' then '-' else C.toLower c)
                     . T.filter (\c -> C.isAlphaNum c || C.isSpace c)
                     $ t
          in "[" <> t <> "](#" <> anchor <> ")"

main :: IO ()
main = do
  args <- getArgs
  if length args == 1
    then printTOC (head args)
    else usage

usage :: IO ()
usage = putStrLn "You must pass a (markdown) filename to the program"
