module ANN.NetworkTraining
( trainNetwork
, testNetwork
) where

import Data.List
import Data.Maybe
import Data.Matrix (Matrix, (<|>), (<->))
import qualified Data.Matrix as M
import qualified Data.Vector as V

import Text.Printf

import ANN.MatrixUtils
import ANN.ActivationFunction
import ANN.Layer
import ANN.Network

trainNetwork :: [ColumnVector Double] -> [ColumnVector Double] -> Network ->
                Double -> Double -> Int -> IO Network
trainNetwork ins outs prc errLim tP i = do
  let training = trainEach trInp trOut prc
      prevValidation = testEachErr vaInp vaOut prc
      validation = testEachErr vaInp vaOut training
      trainingError = testEachErr trInp trOut training
      deltaT = trainingError - testEachErr trInp trOut prc
      deltaV = validation - prevValidation

  putStrLn ("Epoch"++show i++"\terror\tdeltaV\tdeltaT")
  printf "\t%.5f\t" validation
  printf "%.5f\t" deltaV
  printf "%.5f\n\n" deltaT

  if (abs deltaT < 0.005 || abs deltaV < 0.005) && validation < errLim || i > 100
    then return training
    else trainNetwork ins outs training errLim tP (i+1)
  where portion x = round (x * realToFrac (length ins))
        trInp = take (portion tP) ins
        trOut = take (portion tP) outs
        vaInp = drop (portion tP) ins
        vaOut = drop (portion tP) outs

trainEach :: [ColumnVector Double] -> [ColumnVector Double] -> Network ->
             Network
trainEach (i:is) (o:os) prc
  | null is || null os = train prc i o
  | otherwise = trainEach is os (train prc i o)

train :: Network -> ColumnVector Double -> ColumnVector Double ->
         Network
train prc inp out = newPrc
  where pls = propagateNet inp prc
        bpls = backpropagateNet out pls
        newPrc = updateNet prc bpls


testEachErr :: [ColumnVector Double] -> [ColumnVector Double] -> Network ->
               Double
testEachErr (i:is) (o:os) prc
  | null is || null os = testErr i o prc
  | otherwise = (testErr i o prc + testEachErr is os prc)/2

testErr :: ColumnVector Double -> ColumnVector Double -> Network ->
           Double
testErr inp out newPrc = errorM newPls
  where newPls = propagateNet inp newPrc
        errorM pls = magnitude (M.toList (getOutput pls))
        targetList = M.toList out
        magnitude x = sum (map abs (zipWith (-) targetList x))
                      / realToFrac (length x)

testNetwork :: Network -> [ColumnVector Double] -> [Int]
testNetwork _ [] = []
testNetwork prc (d:ds) = testN prc d : testNetwork prc ds

testN :: Network -> ColumnVector Double -> Int
testN prc input = 1 + fromMaybe (-1) (elemIndex (foldl1' max a) a)
  where a = M.toList (getOutput (propagateNet input prc))


roundDouble :: Double -> Int -> Double
roundDouble f n = fromInteger (round $ f * (10^n)) / (10.0^^n)
