-- @project Decision Tree in Haskell
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025
-- @file task2.hs Second task implmentation

module TaskTwo (taskTwo) where

import Common (DecisionTree (EmptyTree, Leaf, Node), replaceComma)
import Data.List (findIndex, maximumBy, minimumBy, sort, transpose)
import Data.Map qualified as Map
import Data.Ord (comparing)

-- Execute the second task of the project
taskTwo :: String -> IO ()
taskTwo trainPath = do
  contentsTrain <- readFile trainPath
  let allCls = map (last . words . replaceComma) $ lines contentsTrain
  let allFeat = map (init . words . replaceComma) $ lines contentsTrain
  let allFeatCls = transpose $ createTuples allFeat allCls
  print $ trainTree allFeatCls [0 .. length (head allFeat) - 1]

-- let feats0 = getFeaturesOnIdx 0 allFeat
-- let clsFeats0 = zip feats0 allCls
-- print $ weightedGini 12 16 0.486 0.375
--

createTuples :: [[String]] -> [String] -> [[(Double, String)]]
createTuples [] [] = []
createTuples (f : fs) (c : cs) = map (\x -> (read x, c)) f : createTuples fs cs

-- Return a threshold (avg) from a list of features
thrAvg :: [(Double, String)] -> Double
thrAvg [] = 0
thrAvg xxs = (head feats + last feats) / 2
  where
    feats = sort $ map fst xxs

trainTree :: [[(Double, String)]] -> [Int] -> DecisionTree
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

getAllSplits :: [[(Double, String)]] -> [(Double, Double, [(Double, String)], [(Double, String)])]
getAllSplits [] = []
getAllSplits allFeatCls = (giniOfSplit, thr, splitA, splitB) : getAllSplits (tail allFeatCls)
  where
    thr = thrAvg $ head allFeatCls
    features = head allFeatCls
    splitA = foldl (\ac f -> if fst f <= thr then f : ac else ac) [] features
    splitB = foldl (\ac f -> if fst f > thr then f : ac else ac) [] features
    giniOfSplit = weightedGini splitA splitB

weightedGini :: [(Double, String)] -> [(Double, String)] -> Double
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

-- thrCandidates :: [Double] -> [Double]
-- thrCandidates [] = []
-- thrCandidates [_] = []
-- thrCandidates (x : y : ys) = ((x + y) / 2) : thrCandidates (y : ys)
--
-- thrAltCandidates :: [Double] -> [Double]
-- thrAltCandidates [] = []
-- thrAltCandidates xxs = [low, low + 0.1 .. high]
--  where
--    low = head xxs
--    high = last xxs
--
