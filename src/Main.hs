module Main where

import Control.Monad.ST
import Data.Vector (Vector, MVector)
import qualified Data.Vector as V
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as MV
import System.Environment (getArgs)
import TextShow
import TextShow.Data.Vector

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No arguments given."
    (x:_) -> printT . sieve $ read x

sieve :: Int -> Vector Int
sieve l = V.map fst . V.filter ((==) True . snd) $ V.zip v marked
  where markIfPrime i vec
          | i >= div (MV.length vec) 2 = return ()
          | otherwise = do
            x <- MV.read vec i
            if x
               then do
                 let step = (v V.! i)
                 markOff vec (i+step) step
                 markIfPrime (i+1) vec
               else markIfPrime (i+1) vec
        v = V.iterateN (l-1) (+1) 2
        markOff vec i step
          | i >= MV.length vec = return ()
          | otherwise = do
            MV.write vec i False
            markOff vec (i+step) step
        marked = runST $ do
          mv <- MV.replicate l True
          markIfPrime 0 mv
          V.freeze mv
