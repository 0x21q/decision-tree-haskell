-- @project Decision Tree in Haskell
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025
-- @file task2.hs Second task implmentation

module TaskTwo (taskTwo) where

import Common (DecisionTree (EmptyTree, Leaf, Node), replaceComma)
import Data.List (maximumBy, minimumBy, sort, transpose)
import Data.Map qualified as Map
import Data.Ord (comparing)

-- Type for a triple which contain (value, class, index)
type FeatCls = (Double, String, Int)

-- Type for quadruple which contain information about split
-- (gini index, best threshold, index, split group A, split group B)
type Split = (Double, Double, Int, [FeatCls], [FeatCls])

-- Execute the second task of the project
taskTwo :: String -> IO ()
taskTwo trainPath = do
  input <- readFile trainPath
  let allFeatCls = parseTrainData $ lines input
  print $ trainTree allFeatCls

-- Parse input training data from and return a list of
-- class pairs for each feature
parseTrainData :: [String] -> [[FeatCls]]
parseTrainData inputLines = transpose $ createTuples allFeat allCls 0
  where
    allCls = map (last . words . replaceComma) inputLines
    allFeat = map (init . words . replaceComma) inputLines

-- Create feature class pairs (tuples) from a list of
-- features and classes parsed from the input
createTuples :: [[String]] -> [String] -> Int -> [[FeatCls]]
createTuples [] _ _ = []
createTuples _ [] _ = []
createTuples (f : fs) (c : cs) acc =
  map (\x -> (read x, c, acc)) f : createTuples fs cs (acc + 1)

-- Main functionality for the training and building of
-- decision tree, takes as input a list of all feature 
-- value class pairs and returns a built decision tree
trainTree :: [[FeatCls]] -> DecisionTree
trainTree [] = EmptyTree
trainTree allFeatCls
  | currentGini <= 0.01 = Leaf $ mostFrequentCls $ concat allFeatCls
  | otherwise = Node idx bestThr left right
  where
    currentGini = giniFromCls . map snd3 . concat $ allFeatCls
    allSplits = getAllSplits allFeatCls 0
    (_, bestThr, idx, spA, spB) = minimumBy (comparing fst5) allSplits
    fullSpA = updateSplit allFeatCls $ map thd3 spA
    fullSpB = updateSplit allFeatCls $ map thd3 spB
    left =
      if null fullSpA
        then Leaf $ mostFrequentCls $ head allFeatCls
        else trainTree fullSpA
    right =
      if null fullSpB
        then Leaf $ mostFrequentCls $ head allFeatCls
        else trainTree fullSpB

-- Get splits of all features which are already splitted with
-- the best threholds for given feature from list of all features
getAllSplits :: [[FeatCls]] -> Int -> [Split]
getAllSplits [] _= []
getAllSplits xxs idx =
  (spGini, bestThr, idx, spA, spB) : getAllSplits (tail xxs) (idx + 1)
  where
    thrs = fixFP . thrCandidates . sort . map fst3 . head $ xxs
    bestThr = getBestThr thrs xxs
    spA = filter (\f -> fst3 f <= bestThr) $ head xxs
    spB = filter (\f -> fst3 f > bestThr) $ head xxs
    spGini = weightedGini spA spB

-- Update split with a corresponding entry values from other features
-- (the input data that are in the same line are added into the split)
updateSplit :: [[FeatCls]] -> [Int] -> [[FeatCls]]
updateSplit allFeatCls indexes = 
  map (filter (\x -> thd3 x `elem` indexes)) allFeatCls

-- Get best threshold from list of threshold candidates
-- based on the gini index of each split
getBestThr :: [Double] -> [[FeatCls]] -> Double
getBestThr [] _ = 1.0
getBestThr xxs allFeatCls =
  fst . minimumBy (comparing snd) . map (\t -> (t, giniOfThr t)) $ xxs
  where
    features = head allFeatCls
    giniOfThr thr = weightedGini splitA splitB
      where
        splitA = filter (\f -> fst3 f <= thr) features
        splitB = filter (\f -> fst3 f > thr) features

-- Calculate weighted gini index of two splitted groups
weightedGini :: [FeatCls] -> [FeatCls] -> Double
weightedGini fA fB = (cntA / cntN) * gA + (cntB / cntN) * gB
  where
    cntA = fromIntegral $ length fA
    cntB = fromIntegral $ length fB
    gA = giniFromCls $ map snd3 fA
    gB = giniFromCls $ map snd3 fB
    cntN = cntA + cntB

-- Based on the list of classes return a map which contains
-- frequencies for each unique class
classFreq :: [String] -> Map.Map String Int -> Map.Map String Int
classFreq [] m = m
classFreq (cls : other) m = classFreq other updated
  where
    updated =
      if Map.member cls m
        then Map.adjust succ cls m
        else Map.insert cls 1 m

-- Calculate gini index from class list
giniFromCls :: [String] -> Double
giniFromCls cls =
  1 - sum (Map.map (\x -> (fromIntegral x / n) ** 2) freqMap)
  where
    n = fromIntegral $ sum freqMap
    freqMap = classFreq cls Map.empty

-- Create threshold candidates using averages of neighbors
-- from a list of feature values (which needs to be sorted)
thrCandidates :: [Double] -> [Double]
thrCandidates [] = []
thrCandidates [_] = []
thrCandidates (x : y : ys) = ((x + y) / 2) : thrCandidates (y : ys)

-- Get the most frequent class from the given split group,
-- this function is generally implemented so that it works
-- even if the base recursion gini value is greater than 0
mostFrequentCls :: [FeatCls] -> String
mostFrequentCls [] = []
mostFrequentCls xxs =
  fst . maximumBy (comparing snd) . Map.toList $ freqMap
  where
    freqMap = classFreq (map snd3 xxs) Map.empty

-- Helper functions for extracting values out of n-sized tuples
fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

snd3 :: (a,b,c) -> b
snd3 (_, x, _) = x

thd3 :: (a,b,c) -> c
thd3 (_, _, x) = x

fst5 :: (a,b,c,d,e) -> a
fst5 (x, _, _, _, _) = x

-- Fix the floating point errors of list of doubles
-- for more clean printing
fixFP :: [Double] -> [Double]
fixFP [] = []
fixFP (x : xs) =
  if length (show x) > 4
    then (read . take 4 . show) x : fixFP xs
    else x : fixFP xs
