{-# LANGUAGE TemplateHaskell #-}
import Control.Monad.LazyRandom
import Control.Monad
import Data.Ratio
import Data.Lens.Lazy
import Data.Lens.Template
import Data.List
import System.Random

data Coin = Heads | Tails deriving (Show, Eq)

data TrialResult = TrialResult {
  _vOne :: Rational,
  _vRand :: Rational,
  _vMin :: Rational,
  _num :: Integer
} deriving (Show, Eq)

$(makeLens ''TrialResult)

mkResult :: Rational -> Rational -> Rational -> TrialResult
mkResult vO vR vM = TrialResult vO vR vM 1

avgResult :: TrialResult -> TrialResult -> TrialResult
avgResult t1 t2 = num ^%= (+n) $ t'
  where t' = foldl' update t2 [vOne, vRand, vMin]
        n = num ^$ t1
        update tr attr = attr ^= incAvg n v1 v2 $ tr
          where v1 = attr ^$ t1
                v2 = attr ^$ t2

incAvg :: Integer -> Rational -> Rational -> Rational
incAvg n a v = v/(nr+1) + (nr*a)/(nr+1)
  where nr = n % 1

isHeads :: Coin -> Bool
isHeads = (== Heads)

count :: (a -> Bool) -> [a] -> Integer
count predicate = toInteger . length . filter predicate

proportion :: (a -> Bool) -> [a] -> Rational
proportion predicate list = count predicate list % toInteger (length list)

coinFlip :: Rand StdGen Coin
coinFlip = do
  n <- getRandom :: Rand StdGen Integer
  return $ if (n `mod` 2) == 0 then Heads else Tails

coinFlips :: Int -> Rand StdGen [Coin]
coinFlips n = replicateM n coinFlip

pHeadsInNFlips :: Int -> Rand StdGen Rational
pHeadsInNFlips n = do
  flips <- coinFlips n
  return . proportion isHeads $ flips

trial :: Rand StdGen TrialResult
trial = do
  coins <- replicateM 1000 (pHeadsInNFlips 10)
  let vO = head coins
  randN <- getRandomR (0, 999)
  let vR = coins !! randN
  let vM = minimum coins
  return $ mkResult vO vR vM

avgNTrials :: Int -> Rand StdGen TrialResult
avgNTrials n = liftM (foldl1' avgResult) . replicateM n $ trial

main :: IO ()
main = do
  gen <- getStdGen
  let result = evalRand (avgNTrials 10000) gen
  print result
