-- | Batch generation of a number of graphs

import System.Cmd
import Text.Printf
import System.Environment
import Data.Maybe

-- | Numbers to generate if no parameter is given
generate :: [Int]
generate = [8..10]

-- | Generates the command for an int
command :: Int -> String
command n = printf "./GraphGenerator %2d | cpp | dot -Tpng > miu%d.png" n n

-- | Runs a command and prints status output
runCommand :: String -> IO ()
runCommand s = do printf "Running %s\n" s
                  system s
                  return ()

main :: IO ()
main = do
      args <- getArgs
      let -- Read first command line parameter for length
          generate' = maybe generate return $ readMaybe =<< listToMaybe args
      mapM_ (runCommand . command) generate'


-- | Because my platform version library doesn't have it yet :-(
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of (a,_):_ -> Just a
                              _else   -> Nothing
