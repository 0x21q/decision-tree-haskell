-- @project Decision Tree in Haskell
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025
-- @file task2.hs Second task implmentation

module TaskTwo (taskTwo) where

import Common (DecisionTree (EmptyTree, Leaf, Node), replaceComma)
import Data.List (maximumBy, minimumBy, sort, transpose)
import Data.Map qualified as Map
import Data.Ord (comparing)

type FeatCls = (Double, String)

-- Execute the second task of the project
taskTwo :: String -> IO ()
taskTwo trainPath = do
  contentsTrain <- readFile trainPath
  let allFeatCls = parseTrainData $ lines contentsTrain
  print $ trainTree allFeatCls 0

  let allSplits = getAllSplits allFeatCls
  let bestSplit = minimumBy (comparing (\(g, _, _, _) -> g)) allSplits
  print allSplits
  putStrLn "\n"
  print bestSplit

parseTrainData :: [String] -> [[FeatCls]]
parseTrainData inputLines = transpose $ createTuples allFeat allCls
  where
    allCls = map (last . words . replaceComma) inputLines
    allFeat = map (init . words . replaceComma) inputLines

createTuples :: [[String]] -> [String] -> [[FeatCls]]
createTuples [] _ = []
createTuples _ [] = []
createTuples (f : fs) (c : cs) = map (\x -> (read x, c)) f : createTuples fs cs

trainTree :: [[FeatCls]] -> Int -> DecisionTree
trainTree [[]] _ = EmptyTree
trainTree allFeatCls c = Node c bestThr left right
  where
    allSplits = getAllSplits allFeatCls
    (gi, bestThr, splitA, splitB) = minimumBy (comparing (\(g, _, _, _) -> g)) allSplits
    left =
      if gi <= 0.3
        then Leaf $ mostFrequentCls splitA
        else trainTree [splitA] (c + 1)
    right =
      if gi <= 0.3
        then Leaf $ mostFrequentCls splitB
        else trainTree [splitB] (c + 1)

mostFrequentCls :: [FeatCls] -> String
mostFrequentCls [] = []
mostFrequentCls xxs =
  fst . maximumBy (comparing snd) . Map.toList $ classFreq (map snd xxs) Map.empty

getAllSplits :: [[FeatCls]] -> [(Double, Double, [FeatCls], [FeatCls])]
getAllSplits [] = []
getAllSplits allFeatCls =
  (giniOfSplit, bestThr, splitA, splitB) : getAllSplits (tail allFeatCls)
  where
    thrs = fixFP . thrCandidates . sort . map fst . head $ allFeatCls
    bestThr = getBestThr thrs allFeatCls
    features = head allFeatCls
    splitA = filter (\f -> fst f <= bestThr) features -- alternatively as fold
    splitB = filter (\f -> fst f > bestThr) features
    giniOfSplit = weightedGini splitA splitB

getBestThr :: [Double] -> [[FeatCls]] -> Double
getBestThr [] _ = 1.0
getBestThr xxs allFeatCls =
  fst . minimumBy (comparing snd) . map (\t -> (t, giniOfThr t)) $ xxs
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

fixFP :: [Double] -> [Double]
fixFP [] = []
fixFP (x : xs) =
  if length (show x) > 4
    then (read . take 4 . show) x : fixFP xs
    else x : fixFP xs
