module Main where

import System.Environment
import qualified System.Random (StdGen)
import qualified System.Random as Random

import Data.List
import Data.Matrix (Matrix, (<|>), (<->))
import qualified Data.Matrix as M
import qualified Data.Vector as V

import ANN.MatrixUtils
import ANN.ActivationFunction
import ANN.Layer
import qualified ANN.Network as Net
import qualified ANN.NetworkTraining as Training

import qualified Parser

numInputNodes  = 400
numHiddenNodes = 9
numOutputNodes = 4

learningRate    = 0.3
errorLimit      = 0.25
trainingPortion = 0.95

-- |The main function.
main :: IO ()
main = do
  (trainingSet:trainingFacit:testSet:_) <- getArgs
  if null trainingSet || null trainingFacit || null testSet
    then error ("Insufficient arguments! This program wants a" ++
                "training set, training facit and a test set.")
    else do
      g <- Random.getStdGen
      trainingSetLines <- Parser.readFileToList trainingSet
      trainingFacitLines <- Parser.readFileToList trainingFacit
      testSetLines <- Parser.readFileToList testSet
      let trainingData = Parser.parse trainingSetLines
          facitData = Parser.parse trainingFacitLines
          testData = Parser.parse testSetLines
          wm1 = newWeightMatrix g numHiddenNodes numInputNodes
          wm2 = newWeightMatrix g numOutputNodes numHiddenNodes
          inp = Parser.prepareInput trainingData --map (M.fromList 100 1 . Parser.sampleImage 1 40 . M.toList . rep 2) (Parser.prepareInput trainingData)
          testInp = Parser.prepareInput testData --map (M.fromList 100 1 . Parser.sampleImage 1 40 . M.toList . rep 2) (Parser.prepareInput testData)
          out = Parser.prepareOutput facitData
          prc = Net.buildNetwork learningRate [wm1,wm2] tanhAS

      putStrLn "Using the following parameters:"
      putStrLn "Network: Input -> 400 -> 9 -> 4 -> Output"
      putStrLn "Learning rate: 0.1"
      putStrLn "Training set portion: 4/5 of input data"
      putStrLn "Validation set portion: 1/5 of input data"
      putStrLn "Training is done if validation error drops below 0.2 or the delta training error drops below 0.0005"
      putStrLn "Validation error has to be at least less than 0.35"
                          -- error minimum limit, train set portion, start epoch
      trainedPrc <- Training.trainNetwork inp out prc errorLimit trainingPortion 1

      let result = Training.testNetwork trainedPrc testInp
      print result
      writeFile
        "./result.txt"
        (intercalate "\n" (Parser.resultFile result (Parser.getImageNameList testData)))

      where rep s m = repV s (repH s m)
            repH s m = foldl1 (<|>) (replicate s m) :: Matrix Double
            repV s m = foldl1 (<->) (replicate s m) :: Matrix Double

{- |
   g = Random generator
   i = input (rows) number
   o = output (cols) number
-}
newWeightMatrix :: Random.StdGen -> Int -> Int -> Matrix Double
newWeightMatrix g o i =
  M.matrix o i $ \(i,j) -> (map (/4.0) (Random.randoms g :: [Double])!!(i*j))*(-1)^(i+j)
