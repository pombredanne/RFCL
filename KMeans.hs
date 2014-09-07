-- kmeans.hs
-- Trevor Pottinger
-- Sat Aug 30 07:48:49 PDT 2014

module KMeans ( Sample(..)
              , WeightedSample(..)
              , Similarity
              , Weight
              , soft_kmeans
              , take_random
              , pairF
              , avg_similarity
              , similarity_matrix
              , weighted_similarity
              -- not specific to kmeans
              , avg
              , std_dev
              -- not useful externally, except for testing
              , apply_sim 
              , derive_centers
              ) where

-- Standard imports
import Control.Exception (assert)
import System.Random (mkStdGen)

-- Third party packages
-- random-shuffle
import System.Random.Shuffle (shuffle')

-- USEFUL funcs
-- similarity_matrix
-- map avg sim_matrix 
-- map std_dev sim_matrix

-- TODO
-- point_query :: [Sample] -> Sample -> Similarity -> [Sample]
-- point_query_count :: [Sample] -> Sample -> Similarity -> Int
-- filter_point_query :: [Sample] -> Sample -> Similarity -> [Sample]

-- TODO repa versions

type Similarity = Double
type Weight = Double

-- TODO how can I define functions for these typeclasses?
--   otherwise I have these ugly `funcs`:
--   (similarity, castToWeighted, castToRaw, (.+), (.*), zero, weighted_zero)
class Sample a 

class WeightedSample a 

avg :: [Similarity] -> Similarity
avg sims =
  (sum sims) / (fromIntegral $ length sims)

std_dev :: [Similarity] -> Similarity
std_dev sims =
  sqrt $ avg $ map (\num -> (num - (avg sims))^2) sims

take_random :: Int -> [a] -> [a]
take_random k samples =
  assert (k <= length samples) 
    take k $ shuffle' samples (length samples) (mkStdGen 148001597)

pairF :: (a -> b -> c) -> (a, b) -> c
pairF f p =
  (fst p) `f` (snd p)

-- desired: soft_kmeans :: [Sample a] -> Int -> [Sample a]
soft_kmeans funcs samples k = 
  let (similarity, castToWeighted, castToRaw, (.+), (.*), zero, weighted_zero) = funcs
  in assert (k > 0 && length samples > 0)
      soft_kmeans_helper funcs samples k (1 - 1/2^20) 30 (replicate k zero) (take_random k samples) 

-- order of parameters such that the first ones remain constant throughout
-- soft_kmeans_helper :: [Sample a] -> Int -> Weight -> Int -> [Sample a] -> [Sample a] -> [Sample a]
soft_kmeans_helper funcs samples k thresh remaining_iterations prev current =
  let (similarity, castToWeighted, castToRaw, (.+), (.*), zero, weighted_zero) = funcs
  -- NOTE this will loop indefinitely if k < 1 or length samples == 0
  in case (remaining_iterations, (avg_similarity similarity prev current) > thresh) of
      (_, True) -> current
      (0, _) -> current
      (i, _) -> soft_kmeans_helper funcs samples k thresh (i-1) current new_centroids
        where
          new_centroids = 
            -- Expectation step
            let sim_matrix = similarity_matrix similarity samples current
            -- Maximization step
            in derive_centers funcs samples sim_matrix

-- apply_sim castToWeighted (.*) samples ==> 
--   takes a weight vector (floats) and returns a weighted sample vector
apply_sim :: (a -> b) -> (Weight -> b -> b) -> [a] -> [Weight] -> [b]
apply_sim castToWeighted mult samples =
  -- WTF why does this need two args?
  let castAndMult x y = (flip $ (flip mult) . castToWeighted) x y
  in (map $ pairF castAndMult) . (flip zip samples)

avg_similarity :: (a -> a -> Similarity) -> [a] -> [a] -> Similarity
avg_similarity similarity samplesA samplesB =
  assert (length samplesA == length samplesB)
    (let len = length samplesA
     in (sum (map (pairF similarity) $ zip samplesA samplesB)) / (fromIntegral len))

-- similarity_matrix :: (Sample a -> Sample a -> Similarity) -> [Sample a] -> [Sample a] -> [[Similarity]]
similarity_matrix :: (a -> a -> Similarity) -> [a] -> [a] -> [[Similarity]]
similarity_matrix similarity samples centroids =
  let sim_vector centroid = map (similarity centroid) samples
  in map sim_vector centroids

-- Ideally no funcs...
-- derive_centers :: [Sample a] -> [[Similarity]] -> [Sample a]
-- Executes the "maximization" step from EM, so that the result are good
--   approximate centers of samples based on their similarities with the
--   current centers.
derive_centers funcs samples sim_matrix =
  -- TODO remove once types don't suck
  let (_, castToWeighted, castToRaw, (.+), (.*), _, weighted_zero) = funcs
  in let weighted_sim_matrix :: [[Similarity]]
         weighted_sim_matrix = weighted_similarity sim_matrix
     in let --weighted_samples :: [[WeightedSample a]]
            weighted_samples = map (apply_sim castToWeighted (.*) samples) weighted_sim_matrix
        in let --new_weighted_centroids :: [WeightedSample a]
               new_weighted_centroids = map (foldl (.+) weighted_zero) weighted_samples
           in map castToRaw new_weighted_centroids

-- this is an optimization to avoid having to do division after reducing a bunch of 
--   weighted samples
weighted_similarity :: [[Similarity]] -> [[Weight]]
weighted_similarity sim_matrix = 
  let div_sum (total_similarity, sim_vec) = map (\sim -> sim / total_similarity) sim_vec
  in map div_sum $ zip (map (fromIntegral . length) sim_matrix) sim_matrix

