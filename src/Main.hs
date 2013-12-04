module Main (main) where

import System.Environment
import Text.Read
import Data.Maybe

import MiuGraph
import Export.GraphVizDot

main :: IO ()
main = do
      args <- getArgs
      case readMaybe =<< listToMaybe args of
            Just limit -> (putStrLn . gvDot) (growAll limit gebStartGraph)
            _ -> putStrLn "Expecting Int parameter for maximum word length"