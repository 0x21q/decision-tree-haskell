-- @project Decision Tree in Haskell
-- @author Jakub Kratochvil (xkrato67)
-- @date March 2025
-- @file main.hs Main file of the project

module Main where

import System.Environment (getArgs)
import TaskOne (taskOne)
import TaskTwo (taskTwo)

-- program entry point
main :: IO ()
main = getArgs >>= parseAndRun

-- Executes given task based on the cmd arguments
parseAndRun :: [String] -> IO ()
parseAndRun ("-1" : treePath : dataPath : _) = taskOne treePath dataPath
parseAndRun ("-2" : trainPath : _) = taskTwo trainPath
parseAndRun _ = putStrLn "[x] Error: Invalid arguments!"
