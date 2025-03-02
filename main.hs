-- @project Decision Trees
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025

import System.Environment (getArgs)
import Data.Char (isSpace)

-- decision tree structure
data DecisionTree
  = EmptyTree
  | Node Int Double DecisionTree DecisionTree
  | Leaf String
  deriving (Show)

-- program entry point
main :: IO ()
main = getArgs >>= parseAndRun

-- Executes given task based on the cmd arguments
parseAndRun :: [String] -> IO ()
parseAndRun (a : b : c : _)
  | a == "-1" = taskOne b c
  | a == "-2" = taskTwo b
  | otherwise = print "Nevalidni argumenty!"
parseAndRun _ = print "Nevalidni argumenty"

-- Executes the first task of the project
taskOne :: String -> String -> IO ()
taskOne treePath dataPath = do
  contentsTree <- readFile treePath
  print $ (buildTree . lines) contentsTree
  readFile dataPath >>= print

-- Build the tree from the lines in a file
buildTree :: [String] -> DecisionTree
buildTree [] = EmptyTree
buildTree allLines = fst $ parseTree 0 allLines

-- Parse tree recursively tracking current indentation level
parseTree :: Int -> [String] -> (DecisionTree, [String])
parseTree _ [] = (EmptyTree, [])
parseTree currentIndent allLines@(line:restLines)
  | null line = parseTree currentIndent restLines  -- Skip empty lines
  | lineIndent < currentIndent = (EmptyTree, allLines)  -- Return to parent
  | lineIndent == currentIndent =
      case words (trim line) of
        ("Node:":rest) -> 
          let featureIdx = read $ filter (/= ',') (head rest) :: Int
              threshold = read $ head $ tail rest :: Double
              (leftChild, remainingAfterLeft) = parseTree (currentIndent + 2) restLines
              (rightChild, remainingAfterRight) = parseTree (currentIndent + 2) remainingAfterLeft
          in (Node featureIdx threshold leftChild rightChild, remainingAfterRight)
        ("Leaf:":rest) -> (Leaf $ head rest, restLines)
        _ -> (EmptyTree, restLines)  -- Unrecognized line
  | otherwise = parseTree currentIndent restLines  -- Skip lines with wrong indentation
  where
    lineIndent = leadSpaceCount line

-- Testing
parseTree' :: Int -> [String] -> DecisionTree
parseTree' _ [] = EmptyTree
parseTree' currentIndent allLines@(line:restLines)
  | null line = parseTree' currentIndent restLines  -- Skip empty lines
  | lineIndent < currentIndent = EmptyTree  -- Return to parent
  | lineIndent == currentIndent =
      case words (trim line) of
        ("Node:":rest) -> 
          let featureIdx = read $ filter (/= ',') (head rest) :: Int
              threshold = read $ head $ tail rest :: Double
              leftChild = parseTree' (currentIndent + 2) restLines
              rightChild = parseTree' (currentIndent + 2) (tail restLines)
          in Node featureIdx threshold leftChild rightChild
        ("Leaf:":rest) -> Leaf $ head rest
        _ -> EmptyTree  -- Unrecognized line
  | otherwise = parseTree' currentIndent restLines  -- Skip lines with wrong indentation
  where
    lineIndent = leadSpaceCount line

leadSpaceCount :: String -> Int
leadSpaceCount [] = 0
leadSpaceCount (x : xs) = if x == ' ' then 1 + leadSpaceCount xs else 0

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

taskTwo :: String -> IO ()
taskTwo _ = print "TODO!!!"
