module Day4 where
  
import Data.List

-- Part 1

policy1 = words

valid :: (String -> [String]) -> String -> Bool
valid policy passphrase = (nub . policy $ passphrase) == policy passphrase

-- Part 2

policy2 = map sort . words
