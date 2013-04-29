{-# LANGUAGE TemplateHaskell #-}
import Data.Lens.Template
import Data.Lens.Lazy
import Data.Packed.Vector
import Data.Ratio
import Data.Packed.Matrix
import Control.Monad
import Control.Monad.LazyRandom
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra

data Point = Point {
  _xCoord :: Double,
  _yCoord :: Double
} deriving (Show, Ord, Eq)

data Function = Function {
  _slope :: Double,
  _offset :: Double
} deriving (Show, Eq)

data Hypothesis  = Hypothesis {
  _xCoeff :: Double,
  _yCoeff :: Double
} deriving (Show, Eq)

data Classification = High | Low deriving (Show, Eq)

$(makeLenses [''Point, ''Function, ''Hypothesis])

class Classifier f where
  classify :: f -> Point -> Classification

instance Classifier Function where
  classify f p = if fy >= p ^. yCoord then Low else High
    where fy = fAtX f $ p ^. xCoord

instance Classifier Hypothesis where
  classify h p = if res < 0 then Low else High
    where res = h ^. xCoeff * p ^. xCoord + h ^. yCoeff * p ^. yCoord

generateFunction :: Rand StdGen Function
generateFunction = do
  p1 <- generatePoint
  p2 <- generatePoint
  let slp = (p2 ^. yCoord - p1 ^. yCoord) / (p2 ^. xCoord - p1 ^. xCoord)
  let ofst = (slp * p1 ^. xCoord) - p1 ^. yCoord
  return $ Function slp ofst

fAtX :: Function -> Double -> Double
fAtX f x = f ^. slope * x + f ^. offset

generatePoint :: Rand StdGen Point
generatePoint = do
  x <- getRandomR (-1, 1)
  y <- getRandomR (-1, 1)
  return $ Point x y

generateNPoints :: Int -> Rand StdGen [Point]
generateNPoints n = replicateM n generatePoint

pointToVector :: Point -> Vector Double
pointToVector p = fromList . map (p^.) $ [xCoord, yCoord]

pointsToMatrix :: [Point] -> Matrix Double
pointsToMatrix = fromRows . map pointToVector

classificationToDouble :: Classification -> Double
classificationToDouble High = 1.0
classificationToDouble Low = -1.0

classificationsToVector :: [Classification] -> Vector Double
classificationsToVector = fromList . map classificationToDouble

vectorToHypothesis :: Vector Double -> Hypothesis
vectorToHypothesis v = Hypothesis x y
  where l = toList v
        x = l !! 0
        y = l !! 1

linearRegression :: [Point] -> [Classification] -> Hypothesis
linearRegression ps cs = vectorToHypothesis $ pinv x <> y
  where x = pointsToMatrix ps
        y = classificationsToVector cs

count :: (a -> Bool) -> [a] -> Integer
count predicate = toInteger . length . filter predicate

proportion :: (a -> Bool) -> [a] -> Rational
proportion predicate list = count predicate list % toInteger (length list)

errorRate :: [Classification] -> [Classification] -> Rational
errorRate fcs hcs = proportion (uncurry (==)) . zip fcs $ hcs

trial :: Rand StdGen Rational
trial = do
  trps <- generateNPoints 100
  f  <- generateFunction
  let trcs = map (classify f) trps
  let h = linearRegression trps trcs
  tsps <- generateNPoints 100
  let tscs = map (classify f) tsps
  let hcs = map (classify h) tsps
  return $ errorRate tscs hcs

avgNTrials :: Int -> Rand StdGen Rational
avgNTrials n = liftM average . replicateM n $ trial

average :: [Rational] -> Rational
average xs = sum xs / fromIntegral (length xs)

main :: IO ()
main = evalRandIO (avgNTrials 10000) >>= print

