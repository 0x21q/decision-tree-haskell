-- @project Decision Tree in Haskell
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025
-- @file task2.hs Second task implmentation

module TaskTwo (taskTwo) where

import Common (DecisionTree (EmptyTree, Leaf, Node), replaceComma)
import Data.List (maximumBy, minimumBy, sort, transpose)
import qualified Data.Map as Map
import Data.Ord (comparing)

-- Type for a triple where: FeatCls = (value, class, index)
type FeatCls = (Double, String, Int)

-- Type for quadruple which contain information about split where:
-- Split = (gini index, best threshold, index, left split, right split)
type Split = (Double, Double, Int, [FeatCls], [FeatCls])

-- Execute the second task of the project
-- @param String path to the train file
-- @return IO () print trained tree
taskTwo :: String -> IO ()
taskTwo trainPath = do
  input <- readFile trainPath
  let allFeatCls = parseTrainData $ lines input
  print $ trainTree allFeatCls

-- Parse input training data into predefined types
-- @param [String] list of input lines
-- @return [[FeatCls]] list of FeatCls triples for each feature
parseTrainData :: [String] -> [[FeatCls]]
parseTrainData inputLines = transpose $ createTriples allFeat allCls 0
  where
    allCls = map (last . words . replaceComma) inputLines
    allFeat = map (init . words . replaceComma) inputLines

-- Create feature class triples from a list of
-- features and classes parsed from the input
-- @param [[String]] list of feature values for each feature
-- @param [String] list of all classes
-- @param Int internal index for given triple (used in updateSplit)
-- @return [[FeatCls]] list of FeatCls triples for each feature
createTriples :: [[String]] -> [String] -> Int -> [[FeatCls]]
createTriples [] _ _ = []
createTriples _ [] _ = []
createTriples (f : fs) (c : cs) acc =
  map (\x -> (read x, c, acc)) f : createTriples fs cs (acc + 1)

-- Main functionality for the training and building of
-- decision tree
-- @param [[FeatCls]] list of FeatCls triples for each feature
-- @return DecisionTree trained decision tree
trainTree :: [[FeatCls]] -> DecisionTree
trainTree [] = EmptyTree
trainTree allFeatCls
  | currentGini < 0.01 = Leaf $ mostFrequentCls $ concat allFeatCls
  | otherwise = Node idx bestThr left right
  where
    currentGini = giniFromCls . map snd3 . concat $ allFeatCls
    allSplits = getAllSplits allFeatCls 0
    (_, bestThr, idx, spL, spR) = minimumBy (comparing fst5) allSplits
    fullSpL = updateSplit allFeatCls $ map thd3 spL
    fullSpR = updateSplit allFeatCls $ map thd3 spR
    left =
      if null fullSpL
        then Leaf $ mostFrequentCls $ head allFeatCls
        else trainTree fullSpL
    right =
      if null fullSpR
        then Leaf $ mostFrequentCls $ head allFeatCls
        else trainTree fullSpR

-- Get splits of all features which are already splitted with
-- the best threhold for given feature from list of all features
-- @param [[FeatCls]] list of FeatCls triples for each feature
-- @param Int initial index of a split
-- @return [Split] list of all splits splitted by their best threshold
getAllSplits :: [[FeatCls]] -> Int -> [Split]
getAllSplits [] _ = []
getAllSplits xxs idx =
  (spGini, bestThr, idx, spA, spB) : getAllSplits (tail xxs) (idx + 1)
  where
    thrs = thrCandidates . sort . map fst3 . head $ xxs
    bestThr = getBestThr thrs xxs
    spA = filter (\f -> fst3 f <= bestThr) $ head xxs
    spB = filter (\f -> fst3 f > bestThr) $ head xxs
    spGini = weightedGini spA spB

-- Update split with a corresponding entry values from other features
-- (the input data that are in the same line are added into the split)
-- @param [[FeatCls]] list of FeatCls triples for each feature
-- @param [Int] list of indexes present in a split of one feature
-- @return [[FeatCls]] updated list of FeatCls triples which share same id
updateSplit :: [[FeatCls]] -> [Int] -> [[FeatCls]]
updateSplit allFeatCls identifiers =
  map (filter (\x -> thd3 x `elem` identifiers)) allFeatCls

-- Get best threshold from list of threshold candidates
-- based on the gini index of each split
-- @param [Double] list of threshold candidates
-- @param [[FeatCls]] list of FeatCls triples for each feature
-- @param Double best threshold for given split
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
-- @param [FeatCls] left split (A)
-- @param [FeatCls] right split (B)
-- @return Double weighted gini index of two splitted groups
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
-- @param [String] list of classes
-- @param Map String Int initial map
-- @return Map String Int map filled with frequencies
classFreq :: [String] -> Map.Map String Int -> Map.Map String Int
classFreq [] m = m
classFreq (cls : other) m = classFreq other updated
  where
    updated =
      if Map.member cls m
        then Map.adjust succ cls m
        else Map.insert cls 1 m

-- Calculate gini index from class list
-- @param [String] list of classes
-- @return Double gini index of classes
giniFromCls :: [String] -> Double
giniFromCls cls =
  1 - sum (map (\x -> (fromIntegral x / n) ** 2) freqMapElems)
  where
    freqMapElems = Map.elems $ classFreq cls Map.empty
    n = fromIntegral $ sum freqMapElems

-- Create threshold candidates using averages of neighbors
-- from a list of feature values (which needs to be sorted)
-- @param [Double] list of feature values
-- @return [Double] list of threshold candidates
thrCandidates :: [Double] -> [Double]
thrCandidates [] = []
thrCandidates [_] = []
thrCandidates (x : y : ys) = ((x + y) / 2) : thrCandidates (y : ys)

-- Get the most frequent class from the given split group,
-- this function is generally implemented so that it works
-- even if the base recursion gini value is greater than 0
-- @param [FeatCls] list of FeatCls triples
-- @return String most frequent class
mostFrequentCls :: [FeatCls] -> String
mostFrequentCls [] = []
mostFrequentCls xxs =
  fst . maximumBy (comparing snd) . Map.toList $ freqMap
  where
    freqMap = classFreq (map snd3 xxs) Map.empty

-- Helper functions for extracting values out of n-sized tuples
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

fst5 :: (a, b, c, d, e) -> a
fst5 (x, _, _, _, _) = x
