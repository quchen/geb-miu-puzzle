import System.Environment
import Data.List
import Data.Maybe
import Text.Printf
import Data.Set (Set)
import qualified Data.Set as S

data MUIAtom = I | M | U
      deriving (Eq, Ord, Show)

data Reduce = MX | PU | UU | III
      deriving (Eq, Ord)

instance Show Reduce where
      show MX  = "mx"
      show PU  = "pu"
      show UU  = "uu"
      show III = "iii"

data MUIExpr = MUIExpr [MUIAtom]
      deriving (Eq, Ord)

-- [M,U,I] -> "MUI"
instance Show MUIExpr where
      show (MUIExpr x) = x >>= show

-- | Generates all possible outgoing edges from MUIExpr
ruleAll :: MUIExpr -> [Edge]
ruleAll (MUIExpr  []) = []
ruleAll mui           = ($ mui) =<< [ ruleAppendU
                                    , ruleDuplicateM
                                    , ruleReduceUU
                                    , ruleReduceIII
                                    ]

-- | Applies the "append U" rule.
--   "If the string ends with I, you may append U."
ruleAppendU :: MUIExpr -> [Edge]
ruleAppendU (MUIExpr []) = []
ruleAppendU mui@(MUIExpr xs) | last xs == I = [Edge (mui, MUIExpr $ xs ++ [U], PU)]
                             | otherwise    = []

-- | Applies the duplication rule.
--   "You may replace Mx with Mxx."
ruleDuplicateM :: MUIExpr -> [Edge]
ruleDuplicateM mui@(MUIExpr (M:xs)) = [Edge (mui, MUIExpr $ M : xs ++ xs, MX)]
ruleDuplicateM _                    = []

-- | Applies U reduction.
--   "You may cancel out UU."
ruleReduceUU :: MUIExpr -> [Edge]
ruleReduceUU mui@(MUIExpr xs) = map makeEdge $ mapMaybe removeUU (splits xs)
      where removeUU [ys, U:U:zs] = Just . MUIExpr $ ys ++ zs
            removeUU _            = Nothing
            makeEdge ys = Edge (mui, ys, UU)

-- | Applies III reduction.
--   "You may replace III with U."
ruleReduceIII :: MUIExpr -> [Edge]
ruleReduceIII mui@(MUIExpr xs) = map makeEdge $ mapMaybe replaceIII (splits xs)
      where replaceIII [ys, I:I:I:zs] = Just . MUIExpr $ ys ++ [U] ++ zs
            replaceIII _              = Nothing
            makeEdge ys = Edge (mui, ys, III)

-- splits [1..5] ==
--     [ [[],[1,2,3,4,5]]
--     , [[1], [2,3,4,5]]
--     , [[1,2], [3,4,5]]
--     , [[1,2,3], [4,5]]
--     , [[1,2,3,4], [5]]
--     , [[1,2,3,4,5],[]]
--     ]
splits :: [a] -> [[[a]]]
splits xs = transpose [inits xs, tails xs]


data Graph = Graph (Set Edge)
      deriving (Eq, Ord)

instance Show Graph where
      show (Graph g) = intercalate "\n" . map show . S.toList $ g

data Edge = Edge (MUIExpr, MUIExpr, Reduce)
      deriving (Eq, Ord)

instance Show Edge where
      show (Edge (start, end, t)) = printf "%s -> %s %s;" (show start)
                                                          (show end)
                                                          (show t)

-- | Grows a graph by evaluating rules for all end notes
graphGrow :: Int -> Graph -> Graph
graphGrow maxLength g@(Graph edges) = Graph $ edges `S.union` S.fromList newEdgesFiltered
      where ends = quickNub . S.toList $ endNodes g
            newEdges = ends >>= ruleAll
            newEdgesFiltered = filter constrainLength newEdges
            constrainLength (Edge (_, MUIExpr xs, _)) | length xs <= maxLength = True
                                                      | otherwise              = False
            quickNub = map head . group . sort

-- | List of all nodes pointed to in the graph
endNodes :: Graph -> Set MUIExpr
endNodes (Graph edges) = S.map (\ ~(Edge (_,x,_)) -> x) edges

-- | Calculates x so that f x == x. Bottom if no such x exists.
fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f x | fx == x = x
               | otherwise = fixedPoint f fx
               where fx = f x

-- | Deletes the first edge that is required as a seed to grow the graph
deleteStartEdge :: Graph -> Graph
deleteStartEdge (Graph edges) = Graph (S.filter notFirstEdge edges)
      where notFirstEdge (Edge (MUIExpr [M,M], _, _)) = False
            notFirstEdge _                            = True

startGraph :: Graph
startGraph = Graph . S.singleton $ Edge (MUIExpr [M,M], MUIExpr [M,I], MX)

-- | Because my platform version library doesn't have it yet :-(
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of []      -> Nothing
                              (a,_):_ -> Just a

addBoilerplate :: IO a -> IO ()
addBoilerplate io = do
      putStrLn "#define mx  [color = greenyellow, labelfontcolor = greenyellow]"
      putStrLn "#define pu  [color = green4, labelfontcolor = green4]"
      putStrLn "#define uu  [color = orangered, labelfontcolor = orangered]"
      putStrLn "#define iii [color = orange, labelfontcolor = orange]"
      putStrLn "digraph G {"
      putStrLn "node [shape = box, color = gray, fontname = \"Courier\"];"
      putStrLn "MI [color = skyblue, style = filled];"
      putStrLn "MU [color = skyblue, style = filled];"
      putStrLn "subgraph cluster_legend {"
      putStrLn "\tlabel = \"Legend\";"
      putStrLn "\tMx -> Mxx mx;"
      putStrLn "\tMxIIIy -> MxUy iii;"
      putStrLn "\tMxI -> MxIU pu;"
      putStrLn "\tMxUUy -> Mxy uu;"
      putStrLn "}"
      io
      putStrLn "}"

main :: IO ()
main = do
      args <- getArgs
      let -- Read first command line parameter for max length; default = 8
          maxLength = fromMaybe 8 $ readMaybe =<< listToMaybe args
          graphFix = fixedPoint (graphGrow maxLength) startGraph
      addBoilerplate . print . deleteStartEdge $ graphFix

