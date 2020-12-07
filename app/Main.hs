module Main where

import qualified Day1.Main as D1
import qualified Day2.Main as D2
import qualified Day3.Main as D3
import qualified Day4.Main as D4
import qualified Day5.Main as D5
import qualified Day6.Main as D6
import qualified Day7.Main as D7

import System.Environment ( getArgs )

main :: IO ()
main = do
  [arg] <- getArgs
  case arg of 
    "1" -> D1.solution1 *> D1.solution2
    "2" -> D2.solution1 *> D2.solution2
    "3" -> D3.solution1 *> D3.solution2
    "4" -> D4.solution1 *> D4.solution2
    "5" -> D5.solution1 *> D5.solution2
    "6" -> D6.solution1 *> D6.solution2
    "7" -> D7.solution1 *> D7.solution2
    _ -> print "No puzzle matched"