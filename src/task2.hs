-- @project Decision Tree in Haskell
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025
-- @file task2.hs Includes second task implmentation

module TaskTwo (taskTwo) where

import qualified Data.Map as Map
import Common (replaceComma)
import Data.List (sort)

taskTwo :: String -> IO ()
taskTwo trainPath = do
  contentsTrain <- readFile trainPath
  let classes = map (last . words . replaceComma) $ lines contentsTrain
  let features = map ( init . words . replaceComma) $ lines contentsTrain
  print $ getFeatures 0 features
  print $ thrCandidates $ getFeatures 0 features
  print $ calcGini $ classFreq classes Map.empty

classFreq :: [String] -> Map.Map String Int -> Map.Map String Int
classFreq [] m = m
classFreq (cls : other) m = classFreq other updated
    where updated = if Map.member cls m
                    then Map.adjust succ cls m
                    else Map.insert cls 1 m

calcGini :: Map.Map String Int -> Double
calcGini m = 1 - sum (Map.map (\x -> (fromIntegral x / fromIntegral (sum m))**2) m)

getFeatures :: Int -> [[String]] -> [Double]
getFeatures _ [] = []
getFeatures idx (line : other) = sort $ read (line !! idx) : getFeatures idx other

thrCandidates :: [Double] -> [Double]
thrCandidates [] = []
thrCandidates [_] = []
thrCandidates (x : y : ys) = ((x + y) / 2) : thrCandidates (y:ys)

