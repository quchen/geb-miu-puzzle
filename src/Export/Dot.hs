-- | Dot file for rendering with GraphViz

module Export.Dot (gvDot, exportDot) where

import qualified Data.Set as Set
import qualified Data.Foldable as F
import Text.Printf

import Miu
import MiuGraph



graphToDot :: (Rule -> String) -> Graph -> String
graphToDot ruleToDot (Graph edges) =
      F.fold (Set.map (edgeToDot ruleToDot) edges)



edgeToDot :: (Rule -> String) -> Edge -> String
edgeToDot ruleToDot (Edge start end rule) =
      printf "\t%s -> %s %s;\n"
            (miuToDot start)
            (miuToDot end)
            (ruleToDot rule)



miuToDot :: MiuExpr -> String
miuToDot (MiuExpr miu) = concatMap atomToDot miu



atomToDot :: MiuAtom -> String
atomToDot M = "M"
atomToDot I = "I"
atomToDot U = "U"



-- | Conversion for plotting the dot file with GraphViz
ruleToDotGV :: Rule -> String
ruleToDotGV MX  = "[color = greenyellow, labelfontcolor = greenyellow]"
ruleToDotGV PU  = "[color = green4,      labelfontcolor = green4]"
ruleToDotGV UU  = "[color = orangered,   labelfontcolor = orangered]"
ruleToDotGV III = "[color = orange,      labelfontcolor = orange]"



-- | Conversion for exporting the dot file for use in other graph programs
ruleToDotExport :: Rule -> String
ruleToDotExport MX  = "[label = mx,  color = greenyellow, labelfontcolor = greenyellow]"
ruleToDotExport PU  = "[label = pu,  color = green4,      labelfontcolor = green4]"
ruleToDotExport UU  = "[label = uu,  color = orangered,   labelfontcolor = orangered]"
ruleToDotExport III = "[label = iii, color = orange,      labelfontcolor = orange]"



addBoilerplateGV :: String -> String
addBoilerplateGV body = unlines
      [ "digraph G {"
      , "\tnode [shape = box, color = gray, fontname = \"Courier\"];"
      , "\tMI [color = skyblue, style = filled];"
      , "\tMU [color = skyblue, style = filled];"
      , "\tsubgraph cluster_legend {"
      , "\t\tlabel = \"Legend\";"
      , "\t\tMx -> Mxx "      ++ ruleToDotGV MX  ++ ";"
      , "\t\tMxIIIy -> MxUy " ++ ruleToDotGV III ++ ";"
      , "\t\tMxI -> MxIU "    ++ ruleToDotGV PU  ++ ";"
      , "\t\tMxUUy -> Mxy "   ++ ruleToDotGV UU  ++ ";"
      , "\t}"
      , ""
      , body
      , "}"
      ]

gvDot :: Graph -> String
gvDot = addBoilerplateGV . graphToDot ruleToDotGV




addBoiletplateExport :: String -> String
addBoiletplateExport body = unlines
      [ "digraph G {"
      , body
      , "}"
      ]

exportDot = addBoiletplateExport . graphToDot ruleToDotExport