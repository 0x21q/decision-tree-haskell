-- @project Decision Tree in Haskell
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025
-- @file common.hs Common types and functions

module Common where

-- Decision tree structure
data DecisionTree
  = EmptyTree
  | Node Int Double DecisionTree DecisionTree
  | Leaf String
  deriving (Show)

-- Number of leading spaces in a string
leadingSpaceCount :: String -> Int
leadingSpaceCount [] = 0
leadingSpaceCount (x : xs) =
  if x == ' ' then 1 + leadingSpaceCount xs else 0

-- Replace commas with spaces in a string
replaceComma :: String -> String
replaceComma [] = []
replaceComma (x : xs) =
  if x == ',' then ' ' : replaceComma xs else x : replaceComma xs
