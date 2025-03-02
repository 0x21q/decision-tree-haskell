-- @project Decision Trees
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025

import System.Environment

data DecisionTree
  = EmptyTree -- might be useless
  | Node Int Double DecisionTree DecisionTree
  | Leaf String
  deriving (Show)

-- processFile :: FilePath -> IO String
-- processFile path = do readFile path

main :: IO ()
main = getArgs >>= parseArguments

parseArguments :: [String] -> IO ()
parseArguments (a : b : c : _)
  | a == "-1" = taskOne b c
  | a == "-2" = taskTwo b
  | otherwise = print "Nevalidni argumenty!"
parseArguments _ = print "Nevalidni argumenty"

taskOne :: String -> String -> IO ()
taskOne treePath dataPath = do
  contentsTree <- readFile treePath
  print $ (buildTree . lines) contentsTree
  readFile dataPath >>= print

buildTree :: [String] -> DecisionTree
buildTree = foldl (\t line -> parseLine line t) EmptyTree

parseLine :: String -> DecisionTree -> DecisionTree
parseLine line =
  -- nejak tu rozhnodnout jestli to je node nebo leaf a taky bych mel trimovat ty splitted
  insertNode splitted cnt
  where
    splitted = words line
    cnt = leadSpaceCount line

-- insertNode :: [String] -> Int -> DecisionTree -> DecisionTree
-- insertNode (_ : x : xs) _ _ = Node (read x) (read $ head xs) EmptyTree EmptyTree
-- insertNode (_ : xs) _ EmptyTree = Leaf $ head xs
-- insertNode [] _ EmptyTree = EmptyTree

insertNode :: [String] -> Int -> DecisionTree -> DecisionTree
insertNode ("Node:" : x : xs) _ EmptyTree =
  Node idx thr EmptyTree EmptyTree
  where
    idx = read $ filter (/= ',') x :: Int
    thr = read $ head xs :: Double
insertNode ("Leaf:" : xs) _ _ = Leaf $ head xs
insertNode _ _ tree = tree

leadSpaceCount :: String -> Int
leadSpaceCount [] = 0
leadSpaceCount (x : xs) = if x == ' ' then 1 + leadSpaceCount xs else 0

taskTwo :: String -> IO ()
taskTwo _ = print "TODO!!!"
