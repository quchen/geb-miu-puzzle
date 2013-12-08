{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment
import Text.Read
import Data.Maybe
import Control.Applicative
import Data.Functor

import MiuGraph
import Export.Dot

main :: IO ()
main = do
      parse <$> getArgs >>= \case
            Just (t, maxLength) -> makeGraph t maxLength
            _else -> help

data GraphType = GVDot | ExportDot
      deriving (Eq, Ord, Show)

-- | Silly handwritten CMD args parser. No dependency etc :-)
parse :: [String] -> Maybe (GraphType, Int)
parse [t, i] = liftA2 (,) (parseType t) (parseInt i)
parse _ = Nothing

parseType :: String -> Maybe GraphType
parseType "gv"  = Just GVDot
parseType "dot" = Just ExportDot
parseType _else = Nothing

parseInt :: String -> Maybe Int
parseInt = readMaybe

help :: IO ()
help = putStrLn "Usage: ./GraphGenerator <gv | dot> <max length :: Int>"

makeGraph :: GraphType -> Int -> IO ()
makeGraph t limit = (putStrLn . converter) (growAll limit gebStart)
      where converter = case t of
                  GVDot     -> gvDot
                  ExportDot -> exportDot
