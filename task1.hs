-- @project Decision Tree in Haskell
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025
-- @file task1.hs Includes first task implmentation

module TaskOne (taskOne) where

-- Decision tree structure
data DecisionTree
  = EmptyTree
  | Node Int Double DecisionTree DecisionTree
  | Leaf String
  deriving (Show)

-- Execute the first task of the project
taskOne :: String -> String -> IO ()
taskOne treePath dataPath = do
  contentsTree <- readFile treePath
  let tree = buildTree $ lines contentsTree
  -- print tree
  contentsData <- readFile dataPath
  mapM_ putStrLn $ classifyAll (lines contentsData) tree

-- Build the tree from the lines in a file
buildTree :: [String] -> DecisionTree
buildTree [] = EmptyTree
buildTree allLines = parseTree 0 allLines

-- Parse lines and generates nodes
parseTree :: Int -> [String] -> DecisionTree
parseTree _ [] = EmptyTree
parseTree hCurr (line : other)
  | hLine == hCurr = generateNode hCurr (line : other)
  | otherwise = parseTree hCurr other
  where
    hLine = leadingSpaceCount line

-- Generate node or leaf of the decision tree
generateNode :: Int -> [String] -> DecisionTree
generateNode hCurr (line : other) =
  case words $ dropWhile (== ' ') line of
    ("Node:" : idxS : thrS) ->
      Node (read $ init idxS) (read $ head thrS) left right
      where
        left = parseTree (hCurr + 2) other
        right = parseTree (hCurr + 2) $ tail other
    ("Leaf:" : cls) -> Leaf $ head cls
    _ -> EmptyTree
generateNode _ _ = EmptyTree

-- Classify all lines of features with classifyLine function
classifyAll :: [String] -> DecisionTree -> [String]
classifyAll [] _ = []
classifyAll _ EmptyTree = []
classifyAll (line : other) tree =
  classifyLine (words $ replaceComma line) tree ++ classifyAll other tree

-- Classify single line given from classifyAll
classifyLine :: [String] -> DecisionTree -> [String]
classifyLine _ (Leaf cls) = [cls]
classifyLine (val : vals) (Node _ thr left right) =
  if read val <= thr then classifyLine vals left else classifyLine vals right
classifyLine _ _ = []

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
