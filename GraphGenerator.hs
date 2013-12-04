import System.Environment
import Data.List
import Data.Maybe
import Control.Monad
import Text.Printf
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Monoid
import Text.Read

-- | Letters in the MIU alphabet.
--
--   Although not necessary in the book since all words start with "M" and have
--   no other "M", it is encoded here explicitly to make the program more
--   flexible.
data MiuAtom = M | I | U
      deriving (Eq, Ord, Show)

-- | Used to tag reduction steps.
data Rule = MX  -- ^ Mx   -> Mxx  ("duplicate")
          | PU  -- ^ xI   -> xIU  (PU = "produce U")
          | UU  -- ^ xUUy -> xy   ("eliminate double U")
          | III -- ^ xIIIy -> xUy ("convert triple I")
      deriving (Eq, Ord, Show)

newtype MiuExpr = MiuExpr [MiuAtom]
      deriving (Eq, Ord, Show)



-- | Calculate all "MiuExpr"s that can be obtained by applying one rule once.
--
--   The result may contain duplicate entries, for example reducing "U""U""U"
--   yields one entry for eliminating each pair.
ruleAll :: MiuExpr -> Set (MiuExpr, Rule)
ruleAll (MiuExpr mui) = (Set.fromList . mapMaybe applyRule . splits) mui
      where
            applyRule (xs, [I])      = Just (MiuExpr (xs ++ [I,U]          ) , PU)
            applyRule (xs, U:U:ys)   = Just (MiuExpr (xs ++ ys             ) , UU)
            applyRule (xs, M:ys)     = Just (MiuExpr (xs ++ [M] ++ ys ++ ys) , MX)
            applyRule (xs, I:I:I:ys) = Just (MiuExpr (xs ++ [U] ++ ys      ) , III)
            applyRule _              = Nothing



-- | List of all possible ways cut a list in two pieces.
--
--   > splits [1..5] ==
--   >    [ [[],[1,2,3,4,5]]
--   >    , [[1], [2,3,4,5]]
--   >    , [[1,2], [3,4,5]]
--   >    , [[1,2,3], [4,5]]
--   >    , [[1,2,3,4], [5]]
--   >    , [[1,2,3,4,5],[]]
--   >    ]
splits :: [a] -> [([a],[a])]
splits xs = zip (inits xs) (tails xs)

-- ==================


-- | A graph consists of:
--
--   1. A set of unvisited nodes
--   2. A set of already visited nodes
--   3. A set of edges
data Graph = Graph (Set Node)
                   (Set Node)
                   (Set Edge)
      deriving (Eq, Ord, Show)

type Node = MiuExpr
data Edge = Edge Node Node Rule
      deriving (Eq, Ord, Show)



-- | Construct all outgoing edges from a given start node
growNode :: Node -> Set Edge
growNode from = Set.map toEdge (ruleAll from)
      where toEdge (miu, rule) = Edge from miu rule


-- | Grow each unvisited node in a graph
growGraph :: Int -> Graph -> Graph
growGraph limit (Graph unvisited visited edges) = Graph unvisited' visited' edges'
      where
            -- New edges: grow all nodes and flatten the structure
            edges' = edges <> newEdges
            newEdges = (constrainLength . F.fold) (Set.map growNode unvisited)
            constrainLength = Set.filter underLimit
            underLimit (Edge _ (MiuExpr mui) _) = length mui <= limit

            -- Add unvisited nodes to the visited ones
            visited' = unvisited <> visited

            -- New unvisited nodes: all edge end points that are not visited'
            unvisited' = Set.map getEnd edges' `Set.difference` visited'
            getEnd (Edge _ e _) = e



-- | Grow a graph until no unvisited nodes remain
growAll limit g = let g'@(Graph n _ _ ) = growGraph limit g
                  in  if Set.null n then g' else growAll limit g'


-- | Starting point of the book's puzzle
gebStartGraph = Graph (Set.singleton (MiuExpr [M,I])) Set.empty Set.empty


-- ==================


-- | Like "Show", but for printing data in the Dot file format.
class ToDot a where
      toDot :: a -> String

instance ToDot Graph where
      toDot (Graph _ _ edges) = F.fold (Set.map toDot edges)

instance ToDot Edge where
      toDot (Edge start end rule) =
            printf "\t%s -> %s %s;\n" (toDot start)
                                      (toDot end)
                                      (toDot rule)

instance ToDot MiuExpr where
      toDot (MiuExpr miu) = concatMap toDot miu

instance ToDot MiuAtom where
      toDot M = "M"
      toDot I = "I"
      toDot U = "U"

instance ToDot Rule where
      toDot MX  = "mx"
      toDot PU  = "pu"
      toDot UU  = "uu"
      toDot III = "iii"

addBoilerplate :: String -> String
addBoilerplate body = unlines
      [ "#define mx  [color = greenyellow, labelfontcolor = greenyellow]"
      , "#define pu  [color = green4, labelfontcolor = green4]"
      , "#define uu  [color = orangered, labelfontcolor = orangered]"
      , "#define iii [color = orange, labelfontcolor = orange]"
      , "digraph G {"
      , "\tnode [shape = box, color = gray, fontname = \"Courier\"];"
      , "\tMI [color = skyblue, style = filled];"
      , "\tMU [color = skyblue, style = filled];"
      , "\tsubgraph cluster_legend {"
      , "\t\tlabel = \"Legend\";"
      , "\t\tMx -> Mxx mx;"
      , "\t\tMxIIIy -> MxUy iii;"
      , "\t\tMxI -> MxIU pu;"
      , "\t\tMxUUy -> Mxy uu;"
      , "\t}"
      , ""
      , body
      , "}"
      ]

-- ==================


main :: IO ()
main = do
      args <- getArgs
      case readMaybe =<< listToMaybe args of
            Just limit -> (putStrLn . addBoilerplate . toDot) (growAll limit gebStartGraph)
            _ -> putStrLn "Expecting Int parameter for maximum word length"