module Main where

import System.Environment
import Data.Time.Clock

import ReadScene

main = do args <- getArgs
          start <- getCurrentTime
          renderScene $ args !! 0
          end <- getCurrentTime
          print $ diffUTCTime end start
          

