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
              , logSimilarity
              ) where

import qualified KMeans

type SimpleInt = Int
type SimpleWeightedInt = Double

instance KMeans.Sample SimpleInt
instance KMeans.WeightedSample SimpleWeightedInt

logDistance a b = log $ 1 + (abs $ fromIntegral $ a - b)

logSimilarity e a b = exp $ e * logDistance a b

similarity :: SimpleInt -> SimpleInt -> KMeans.Similarity
similarity = logSimilarity (-1/(2^20))

castToWeighted :: SimpleInt -> SimpleWeightedInt
castToWeighted sample = fromIntegral sample

zero :: SimpleInt
zero = 0

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
