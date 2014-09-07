-- SimpleOrb.hs
-- Trevor Pottinger
-- Mon Sep  1 18:42:58 PDT 2014

module Main where

import System.Environment (getArgs)

import qualified KMeans
import qualified Simple
import qualified Group

-- (Time, Action, SourceIP, SourceID)
type SimpleOrb = (Simple.SimpleInt, Group.GroupItem, Group.GroupItem, Group.GroupItem)
type SimpleWeightedOrb = (Simple.SimpleWeightedInt, Group.GroupWeightedItem, Group.GroupWeightedItem, Group.GroupWeightedItem)

instance KMeans.Sample SimpleOrb
instance KMeans.WeightedSample SimpleWeightedOrb

-- Is there a better way to define all the functions? They're essentially
--   mapped versions of what each individual type is...

similarity :: SimpleOrb -> SimpleOrb -> KMeans.Similarity
similarity a b = 
  let ((a1, a2, a3, a4), (b1, b2, b3, b4)) = (a, b)
  -- average of similarities
  in sum [Simple.similarity a1 b1, Group.similarity a2 b2, Group.similarity a3 b3, Group.similarity a4 b4] / 4

castToWeighted :: SimpleOrb -> SimpleWeightedOrb
castToWeighted sample = 
  let (s1, s2, s3, s4) = sample
  in (Simple.castToWeighted s1, Group.castToWeighted s2, Group.castToWeighted s3, Group.castToWeighted s4)

zero :: SimpleOrb
zero = (Simple.zero, Group.zero, Group.zero, Group.zero)

(.+) :: SimpleWeightedOrb -> SimpleWeightedOrb -> SimpleWeightedOrb
(.+) a b = 
  let ((a1, a2, a3, a4), (b1, b2, b3, b4)) = (a, b)
  in (Simple.add a1 b1, Group.add a2 b2, Group.add a3 b3, Group.add a4 b4)
(.*) :: KMeans.Weight -> SimpleWeightedOrb -> SimpleWeightedOrb
(.*) weight w_sample = 
  let (s1, s2, s3, s4) = w_sample
  in (Simple.mult weight s1, Group.mult weight s2, Group.mult weight s3, Group.mult weight s4)

add = (.+)
mult = (.*)

castToRaw :: SimpleWeightedOrb -> SimpleOrb
castToRaw sample = 
  let (s1, s2, s3, s4) = sample
  in (Simple.castToRaw s1, Group.castToRaw s2, Group.castToRaw s3, Group.castToRaw s4)

weighted_zero :: SimpleWeightedOrb
weighted_zero = (Simple.weighted_zero, Group.weighted_zero, Group.weighted_zero, Group.weighted_zero)

-- until the type interface is fixed, this is the minimal requirement to define
--   a new type to be clustered
funcs = (similarity, castToWeighted, castToRaw, (.+), (.*), zero, weighted_zero)

parseLine :: String -> (Int, String, String, String)
parseLine s =
  let [time, action, ip, uid] = words s
  in (read time :: Int, action, ip, uid)
-- map (read :: String -> Int)

main = do
  [file_name, iterations] <- getArgs
  file_contents <- readFile file_name
  putStrLn $ "Running k-means on " ++ file_name ++ " for " ++ iterations ++ " iterations." 
  let samples = map parseLine $ lines file_contents
      clusters = KMeans.soft_kmeans funcs samples (read iterations :: Int)
      sim_matrix = KMeans.similarity_matrix similarity samples clusters
      avgs = map KMeans.avg sim_matrix
      --std_devs = map KMeans.std_dev sim_matrix
  print clusters
  print avgs
  --print std_devs
