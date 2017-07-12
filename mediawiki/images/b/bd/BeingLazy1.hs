module Main where

import Control.Concurrent
import Control.Monad
import Data.List
import System.Random
import Text.Printf

type Score = (Int,Int)

calcScore :: Eq a => [a] -> [a] -> Score
calcScore secret guess = (rightPos, wrongPos)
    where
      rightPos = length [() | (a,b) <- zip secret guess, a==b]
      wrongPos = length secret - length wrongTokens - rightPos
      wrongTokens = guess \\ secret

pool = "rgbywo"

universe :: [String]
universe = perms 4 pool
    where
      perms n p = [s' | s  <- subsequences p, length s == n,
                        s' <- permutations s]

chooseSecret :: IO String
chooseSecret = do
  x <- randomRIO (0, length universe - 1)
  return (universe !! x)

guessSecret :: [Score] -> [String] -> [String]
guessSecret _      []     = []
guessSecret scores (g:gs) =
  g : guessSecret (tail scores)
         [p | p <- gs, calcScore p g == head scores]

main :: IO ()
main = do
  ch <- newChan
  scores <- getChanContents ch
  let guesses = guessSecret scores universe

  forM_ guesses $ \guess -> do
              putStrLn guess
              putStrLn "RightPos?"
              rp <- readLn
              putStrLn "WrongPos?"
              wp <- readLn
              writeChan ch (rp,wp)


main_old :: IO ()
main_old = do
  secret <- chooseSecret
  putStrLn $ "Secret : " ++ secret

  let guesses = guessSecret scores universe
      scores  = map (calcScore secret) guesses
      lines   = zipWith (\g s -> printf "%s %s" g (show s))
                  guesses
                  scores

  putStrLn $ unlines lines