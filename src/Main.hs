{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment
import Text.Read
import Data.Maybe

import MiuGraph
import Export.GraphVizDot

main :: IO ()
main = getArgs >>= listToMaybe >>= readMaybe >>= \case
      Just limit -> (putStrLn . gvDot) (growAll limit gebStartGraph)
      _ -> putStrLn "Expecting Int parameter for maximum word length"