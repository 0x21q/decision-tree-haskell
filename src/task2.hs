-- @project Decision Tree in Haskell
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025
-- @file task2.hs Second task implmentation

module TaskTwo (taskTwo) where

import Common (DecisionTree (EmptyTree, Leaf, Node), replaceComma)
import Data.List (minimumBy, sort, transpose)
import Data.Map qualified as Map
import Data.Ord (comparing)

type FeatCls = (Double, String)

-- Execute the second task of the project
taskTwo :: String -> IO ()
taskTwo trainPath = do
  contentsTrain <- readFile trainPath
  let allFeatCls = parseTrainData $ lines contentsTrain
  let thrs = sort $ map fst $ head allFeatCls
  let thrCs = thrCandidates $ map fst $ head allFeatCls
  let bestThr = getBestThr thrCs allFeatCls
  print thrs
  print thrCs
  print bestThr
  let thrs2 = sort $ map fst $ last allFeatCls
  let thrCs2 = thrCandidates $ map fst $ last allFeatCls
  let bestThr2 = getBestThr thrCs2 allFeatCls
  print thrs2
  print thrCs2
  print bestThr2
  let allSplits = getAllSplits allFeatCls
  let bestSplit = minimumBy (comparing (\(g, _, _, _) -> g)) allSplits
  print allSplits
  print bestSplit
  -- print $ trainTree allFeatCls [0 .. length (head allFeat) - 1]

parseTrainData :: [String] -> [[FeatCls]]
parseTrainData lines = transpose $ createTuples allFeat allCls
  where 
    allCls = map (last . words . replaceComma) lines
    allFeat = map (init . words . replaceComma) lines

createTuples :: [[String]] -> [String] -> [[FeatCls]]
createTuples [] _ = []
createTuples _ [] = []
createTuples (f : fs) (c : cs) = map (\x -> (read x, c)) f : createTuples fs cs

trainTree :: [[FeatCls]] -> [Int] -> DecisionTree
trainTree _ [] = EmptyTree
trainTree allFeatCls [] = Leaf ((snd . head . head) allFeatCls)
trainTree allFeatCls indices = Node bestIdx bestThr left right
  where
    allSplits = getAllSplits allFeatCls
    bestSplit@(_, bestThr, splitA, splitB) = minimumBy (comparing (\(g, _, _, _) -> g)) allSplits
    splitPos = head [i | (i, split) <- zip [0 ..] allSplits, split == bestSplit]
    bestIdx = indices !! splitPos
    left = trainTree [splitA] (tail indices)
    right = trainTree [splitB] (tail indices)

-- trainTree allFeatCls (i : is) = Node i thr left right
--  where
--    ((g, thr, splitA, splitB) : other) = getAllSplits allFeatCls
--    -- find the split that has the smallest gini and use that split to create
--    -- node and recursively build the left and right subtree
--    left = EmptyTree
--    right = EmptyTree
-- trainTree _ _ = EmptyTree

getAllSplits :: [[FeatCls]] -> [(Double, Double, [FeatCls], [FeatCls])]
getAllSplits [] = []
getAllSplits allFeatCls =
  (giniOfSplit, bestThr, splitA, splitB) : getAllSplits (tail allFeatCls)
  where
    thrs = thrCandidates $ (sort . map fst . head) allFeatCls
    bestThr = getBestThr thrs allFeatCls
    features = head allFeatCls
    splitA = filter (\f -> fst f <= bestThr) features -- alternatively as fold
    splitB = filter (\f -> fst f > bestThr) features
    giniOfSplit = weightedGini splitA splitB

getBestThr :: [Double] -> [[FeatCls]] -> Double
getBestThr [] _ = 1.0
getBestThr xxs allFeatCls 
    = fst $ minimumBy (comparing snd) $ map (\t -> (t, giniOfThr t)) xxs
  where
    features = head allFeatCls
    giniOfThr thr = weightedGini splitA splitB
      where 
        splitA = filter (\f -> fst f <= thr) features
        splitB = filter (\f -> fst f > thr) features

weightedGini :: [FeatCls] -> [FeatCls] -> Double
weightedGini fA fB = (cntA / cntN) * gA + (cntB / cntN) * gB
  where
    cntA = fromIntegral $ length fA
    cntB = fromIntegral $ length fB
    gA = giniFromCls $ map snd fA
    gB = giniFromCls $ map snd fB
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

giniFromCls :: [String] -> Double
giniFromCls cls =
  1 - sum (Map.map (\x -> (fromIntegral x / n) ** 2) freqMap)
  where
    n = fromIntegral $ sum freqMap
    freqMap = classFreq cls Map.empty

thrCandidates :: [Double] -> [Double]
thrCandidates [] = []
thrCandidates [_] = []
thrCandidates (x : y : ys) = ((x + y) / 2) : thrCandidates (y : ys)

-- Return a threshold (avg) from a list of features
--thrAvg :: [(Double, String)] -> Double
--thrAvg [] = 0
--thrAvg xxs = (head feats + last feats) / 2
--  where
--    feats = sort $ map fst xxs
