-- @project Decision Tree in Haskell
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025
-- @file task1.hs Includes first task implmentation

module TaskOne (taskOne) where

import Common
  ( DecisionTree (EmptyTree, Leaf, Node),
    leadingSpaceCount,
    replaceComma,
  )

-- Execute the first task of the project
-- @param String path to the tree file
-- @param String path to the data file
-- @return IO () print classification results
taskOne :: String -> String -> IO ()
taskOne treePath dataPath = do
  treeInput <- readFile treePath
  let tree = buildTree $ lines treeInput
  dataInput <- readFile dataPath
  mapM_ putStrLn $ classifyAll (lines dataInput) tree

-- Build the tree from the lines in a file
-- @param [String] list of all input lines
-- @return DecisionTree built decision tree
buildTree :: [String] -> DecisionTree
buildTree [] = EmptyTree
buildTree allLines = parseTree 0 allLines

-- Parse lines and generates nodes
-- @param Int current hierarchy (indent)
-- @param [String] list of remaining lines
-- @return DecisionTree built decision tree
--
-- NOTE: otherwise is needed here since the lines of the
-- right subtree could overlap with left subtree, this case
-- is covered by this since the overlapping lines are skipped
parseTree :: Int -> [String] -> DecisionTree
parseTree _ [] = EmptyTree
parseTree hCurr (line : other)
  | hLine == hCurr = generateNode hCurr (line : other)
  | otherwise = parseTree hCurr other
  where
    hLine = leadingSpaceCount line

-- Generate node or leaf of the decision tree
-- @param Int current hierarchy (indent)
-- @param [String] list of remaining lines
-- @return DecisionTree built decision tree
generateNode :: Int -> [String] -> DecisionTree
generateNode hCurr (line : other) =
  case words $ dropWhile (== ' ') line of
    ("Leaf:" : cls) -> Leaf $ head cls
    ("Node:" : idxStr : thrStr) -> Node idx thr left right
      where
        idx = read $ init idxStr
        thr = read $ head thrStr
        left = parseTree (hCurr + 2) other
        right = parseTree (hCurr + 2) $ tail other
    _ -> EmptyTree
generateNode _ _ = EmptyTree

-- Classify all lines of features with classifyLine function
-- @param [String] list of all input lines
-- @param DecisionTree built decision tree
-- @return [String] list of resulting classes for each line
classifyAll :: [String] -> DecisionTree -> [String]
classifyAll [] _ = []
classifyAll _ EmptyTree = []
classifyAll (line : other) tree =
  classifyLine (words $ replaceComma line) tree : classifyAll other tree

-- Classify single line given from classifyAll
-- @param [String] list of strings from splitted line
-- @param DecisionTree built decision tree
-- @return String resulting class
classifyLine :: [String] -> DecisionTree -> String
classifyLine [] _ = []
classifyLine _ EmptyTree = []
classifyLine _ (Leaf cls) = cls
classifyLine vals (Node i thr left right) =
  if read (vals !! i) <= thr
    then classifyLine vals left
    else classifyLine vals right
