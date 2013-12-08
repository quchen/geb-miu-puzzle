{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment
import Text.Read
import Data.Maybe

import MiuGraph
import Export.Dot

main :: IO ()
main = do
      args <- getArgs
      case listToMaybe args >>= readMaybe of
            Just limit -> (putStrLn . gvDot) (growAll limit gebStart)
            _ -> putStrLn "Expecting Int parameter for maximum word length"