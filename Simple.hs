-- simple.hs
-- Trevor Pottinger
-- Wed Aug 27 23:04:18 PDT 2014

module Simple ( SimpleInt
              , SimpleWeightedInt
              , similarity
              , castToWeighted
              , castToRaw
              , add
              , mult
              , zero
              , weighted_zero
              , funcs
              ) where

-- import System.IO
import System.Environment (getArgs)

import qualified KMeans

type SimpleInt = Int
type SimpleWeightedInt = Double

instance KMeans.Sample SimpleInt
instance KMeans.WeightedSample SimpleWeightedInt

--distance a b = abs $ fromIntegral $ a - b
distance a b = log $ 1 + (abs $ fromIntegral $ a - b)

--instance KMeans.Sample SimpleInt --where
similarity :: SimpleInt -> SimpleInt -> KMeans.Similarity
--similarity a b = exp $ -1/(2^25) * distance a b
similarity a b = exp $ -1/(2^20) * distance a b
--similarity a b = exp $ -(log $ abs $ fromIntegral $ a - b)

castToWeighted :: SimpleInt -> SimpleWeightedInt
castToWeighted sample = fromIntegral sample

zero :: SimpleInt
zero = 0

--instance KMeans.WeightedSample SimpleWeightedInt --where
(.+) :: SimpleWeightedInt -> SimpleWeightedInt -> SimpleWeightedInt
(.+) a b = a + b
(.*) :: KMeans.Weight -> SimpleWeightedInt -> SimpleWeightedInt
(.*) weight w_sample = weight * w_sample

add = (.+)
mult = (.*)

castToRaw :: SimpleWeightedInt -> SimpleInt
castToRaw x = round x

weighted_zero :: SimpleWeightedInt
weighted_zero = 0.0

-- until the type interface is fixed, this is the minimal requirement to define
--   a new type to be clustered
funcs = (similarity, castToWeighted, castToRaw, (.+), (.*), zero, weighted_zero)

-- TODO for more flexibility
-- getSimilarity :: SimpleInt -> SimpleInt -> (SimpleInt -> SimpleInt -> KMeans.Similarity)
-- getDistSimilarity :: SimpleInt -> SimpleInt -> (SimpleInt -> SimpleInt -> KMeans.Similarity)

main = do
  [file_name] <- getArgs
  file_contents <- readFile file_name
  --times = lines file_contents
  print $ KMeans.soft_kmeans funcs (map (read :: String -> Int) $ lines file_contents) 20
