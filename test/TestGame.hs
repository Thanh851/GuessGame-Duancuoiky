module Main where

import Test.HUnit

compareGuess :: Int -> Int -> String
compareGuess secret guess
  | guess < secret = "Too low!"
  | guess > secret = "Too high!"
  | otherwise      = "Correct! You win!"

t1 = TestCase (assertEqual "too low" "Too low!" (compareGuess 50 10))
t2 = TestCase (assertEqual "too high" "Too high!" (compareGuess 50 90))
t3 = TestCase (assertEqual "correct" "Correct! You win!" (compareGuess 42 42))

main :: IO ()
main = do
  counts <- runTestTT (TestList [t1,t2,t3])
  print counts
