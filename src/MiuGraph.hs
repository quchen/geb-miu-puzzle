module MiuGraph (
        growAll
      , gebStartGraph
      , Graph (..)
      , Edge (..)
      , Node
) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Foldable as F
import Data.Monoid

import Miu

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