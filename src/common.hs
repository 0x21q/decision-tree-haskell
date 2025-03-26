-- @project Decision Tree in Haskell
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025
-- @file common.hs Common types and functions

module Common where

-- Decision tree structure
data DecisionTree
  = EmptyTree
  | Leaf String
  | Node Int Double DecisionTree DecisionTree

-- Show implementation for DecisionTree utilizing
-- a helper function printTree to allow recursion
instance Show DecisionTree where
  show = printTree 0

-- Function used in Show implementation for DecisionTree
-- @param Int current indentation
-- @param DecisionTree decision tree to be printed
-- @param String string representation of tree
printTree :: Int -> DecisionTree -> String
printTree _ EmptyTree = []
printTree c (Leaf cls) = replicate c ' ' ++ "Leaf: " ++ cls
printTree c (Node i t l r) =
  replicate c ' '
    ++ "Node: "
    ++ show i
    ++ ", "
    ++ show t
    ++ "\n"
    ++ printTree (c + 2) l
    ++ "\n"
    ++ printTree (c + 2) r

-- Number of leading spaces in a string
-- @param String input string
-- @return Int number of leading spaces
leadingSpaceCount :: String -> Int
leadingSpaceCount [] = 0
leadingSpaceCount (x : xs) =
  if x == ' ' then 1 + leadingSpaceCount xs else 0

-- Replace commas with spaces in a string
-- @param String input string
-- @return String string with ' ' instead of ','
replaceComma :: String -> String
replaceComma [] = []
replaceComma (x : xs) =
  if x == ',' then ' ' : replaceComma xs else x : replaceComma xs
