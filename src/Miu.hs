-- | Elements and rules of the MIU system

module Miu (
        ruleAll
      , MiuAtom (..)
      , Rule (..)
      , MiuExpr (..)
) where

import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

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