module Main where

import           Data.List
import           Data.Maybe

import           System.Environment
import           System.Exit

import qualified Colors             as C
import qualified CurvesAndAnimation as CAA
import qualified Gradients          as G


examples :: [(String, IO ())]
examples =
  [ ("colors",                 C.main)
  , ("gradients",              G.main)
  , ("curves-and-animation", CAA.main)
  ]

main :: IO ()
main = do
  argv <- getArgs
  if null argv then
    die $ "Please supply, as an argument, the example you want to run (" ++ titleList ++ ")."
  else
    fromMaybe
      (die $ "The following examples are available: " ++ titleList)
      (lookup (head argv) examples)
  where
    titleList = intercalate ", " (fst <$> examples)
