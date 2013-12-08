module MiuGraph (
        growAll
      , gebStart
      , Graph (..)
      , Edge (..)
      , Node
) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Foldable as F
import Data.Monoid

import Miu

-- | A graph is defined by the set of its edges only. (Since the MIU graph is
--   connected, this doesn't constrain the set of solutions.)
data Graph = Graph (Set Edge)
      deriving (Eq, Ord, Show)

type Node = MiuExpr
data Edge = Edge Node Node Rule
      deriving (Eq, Ord, Show)

-- | Memoizing version of a graph. Consists of:
--     1. A set of unvisited nodes
--     2. A set of already visited nodes
--     3. A set of edges
type GraphMemo = (Set Node, Set Node, Graph)



-- | Construct all outgoing edges from a given start node
growNode :: Node -> Set Edge
growNode from = Set.map toEdge (ruleAll from)
      where toEdge (miu, rule) = Edge from miu rule


-- | Grow each unvisited node in the graph
growOnce :: Int -- ^ Maximum word length (example: MIIUU = 5)
         -> GraphMemo
         -> GraphMemo
growOnce limit (unvisited,  visited,  Graph edges ) =
               (unvisited', visited', Graph edges')
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
growAll :: Int -- ^ Maximum word length (example: MIIUU = 5)
        -> GraphMemo
        -> Graph
growAll limit gg =
      case growOnce limit gg of
            (unvisited', _, g) | Set.null unvisited' -> g
            gg' -> growAll limit gg'



-- | Starting point of the book's puzzle
gebStart :: GraphMemo
gebStart = (unvisited, visited, graph)
      where unvisited = Set.singleton (MiuExpr [M,I])
            visited = Set.empty
            graph = Graph Set.empty
