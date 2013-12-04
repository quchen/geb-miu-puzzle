-- | Dot file for rendering with GraphViz

module Export.GraphVizDot (gvDot) where

import qualified Data.Set as Set
import qualified Data.Foldable as F
import Text.Printf

import Miu
import MiuGraph



graphToDot :: Graph -> String
graphToDot (Graph _ _ edges) = F.fold (Set.map edgeToDot edges)



edgeToDot :: Edge -> String
edgeToDot (Edge start end rule) = printf "\t%s -> %s %s;\n"
                                         (miuToDot start)
                                         (miuToDot end)
                                         (ruleToDot rule)



miuToDot :: MiuExpr -> String
miuToDot (MiuExpr miu) = concatMap atomToDot miu



atomToDot :: MiuAtom -> String
atomToDot M = "M"
atomToDot I = "I"
atomToDot U = "U"



ruleToDot :: Rule -> String
ruleToDot MX  = "[color = greenyellow, labelfontcolor = greenyellow]"
ruleToDot PU  = "[color = green4,      labelfontcolor = green4]"
ruleToDot UU  = "[color = orangered,   labelfontcolor = orangered]"
ruleToDot III = "[color = orange,      labelfontcolor = orange]"



addBoilerplate :: String -> String
addBoilerplate body = unlines
      [ "digraph G {"
      , "\tnode [shape = box, color = gray, fontname = \"Courier\"];"
      , "\tMI [color = skyblue, style = filled];"
      , "\tMU [color = skyblue, style = filled];"
      , "\tsubgraph cluster_legend {"
      , "\t\tlabel = \"Legend\";"
      , "\t\tMx -> Mxx "      ++ ruleToDot MX  ++ ";"
      , "\t\tMxIIIy -> MxUy " ++ ruleToDot III ++ ";"
      , "\t\tMxI -> MxIU "    ++ ruleToDot PU  ++ ";"
      , "\t\tMxUUy -> Mxy "   ++ ruleToDot UU  ++ ";"
      , "\t}"
      , ""
      , body
      , "}"
      ]

gvDot :: Graph -> String
gvDot = addBoilerplate . graphToDot