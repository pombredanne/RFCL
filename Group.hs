-- Group.hs
-- Trevor Pottinger
-- Mon Sep  1 16:08:45 PDT 2014

module Group ( GroupItem
              , GroupWeightedItem
              , similarity
              , castToWeighted
              , castToRaw
              , add
              , mult
              , zero
              , weighted_zero
              , funcs
              ) where

import Data.Map
import Data.Tuple
import System.Environment (getArgs)

import qualified KMeans

type GroupItem = [Char]
type GroupWeightedItem = Map GroupItem Double

instance KMeans.Sample GroupItem
instance KMeans.WeightedSample GroupWeightedItem

phi = 1.618033988749895
is_match = 1.0 
not_a_match = 1.0 / phi

similarity_helper eq
  | True = is_match
  | otherwise = not_a_match

similarity :: GroupItem -> GroupItem -> KMeans.Similarity
similarity a b = similarity_helper $ a == b

castToWeighted :: GroupItem -> GroupWeightedItem
castToWeighted sample = fromList [(sample, 1.0)]

-- this is weird..
zero :: GroupItem
zero = ""

(.+) :: GroupWeightedItem -> GroupWeightedItem -> GroupWeightedItem
(.+) a b = unionWith (+) a b
(.*) :: KMeans.Weight -> GroupWeightedItem -> GroupWeightedItem
(.*) weight w_sample = Data.Map.map (\count -> weight * count) w_sample

add = (.+)
mult = (.*)

castToRaw :: GroupWeightedItem -> GroupItem
castToRaw x = snd $ findMax $ fromList $ Prelude.map swap $ toList x

weighted_zero :: GroupWeightedItem
weighted_zero = empty

-- until the type interface is fixed, this is the minimal requirement to define
--   a new type to be clustered
funcs = (similarity, castToWeighted, castToRaw, (.+), (.*), zero, weighted_zero)

-- TODO for flexibility
-- getSimilarity :: GroupItem -> GroupItem -> (GroupItem -> GroupItem -> KMeans.Similarity)
-- getMatchSimilarity :: Similarity -> Similarity -> (GroupItem -> GroupItem -> KMeans.Similarity)
-- getMatrixSimilarity :: Map GroupItem (Map GroupItem Int) -> (GroupItem -> GroupItem -> KMeans.Similarity)
-- getSparseMatrixSimilarity :: Map GroupItem (Map GroupItem Int) -> GroupItem -> GroupItem -> (GroupItem -> GroupItem -> KMeans.Similarity)

main = do
  [file_name] <- getArgs
  file_contents <- readFile file_name
  print $ KMeans.soft_kmeans funcs (lines file_contents) 10
