module ANN.Network
( Network(..)
, propagateNet
, backpropagateNet
, buildNetwork
, updateNet
, getOutput
, getWeights
) where

import ANN.MatrixUtils
import ANN.Layer
import ANN.ActivationFunction
import Data.Matrix

data Network = Network
  {
    layers :: [Layer],
    learningRate :: Double
  }

checkDimensions :: Matrix Double -> Matrix Double -> Matrix Double
checkDimensions w1 w2 =
  if nrows w1 == ncols w2
    then w2
    else error "Inconsistent dimensions in weight matrix"

validateInput :: Network -> ColumnVector Double -> ColumnVector Double
validateInput net input =
  if ncols firstlayer == nrows input
    then if withinRange (toList input)
      then input
      else error ("Values in input matrix not within range (0,1) \n" ++ show input)
    else error ("Inconsistent dimensions in input matrix "
                ++ show (ncols firstlayer) ++ " /= " ++ show (nrows input))
  where firstlayer = lW (head (layers net))
        withinRange i = and (scanl inRange (firstInRange i) i)
        firstInRange a = head a >= 0 && head a <= 1
        inRange a b = a && (b >= 0 && b <= 1)

buildNetwork ::
  Double -> [Matrix Double] -> ActivationSpec -> Network
buildNetwork lr ws s = Network { layers=ls, learningRate=lr }
  where checkedWeights = scanl1 checkDimensions ws
        ls = map buildLayer checkedWeights
        buildLayer w = Layer { lW=w, lB=fromList (nrows w) 1 [1..], lAS=s }

propagateNet :: ColumnVector Double -> Network -> [PropagatedLayer]
propagateNet input net = tail calcs
  where calcs = scanl propagate layer0 (layers net)
        layer0 = PropagatedSensorLayer{ pOut=validatedInputs }
        validatedInputs = validateInput net input

backpropagateNet ::
  ColumnVector Double -> [PropagatedLayer] -> [BackpropagatedLayer]
backpropagateNet target layers = scanr backpropagate layerL hiddenLayers
  where hiddenLayers = init layers
        layerL = backpropagateFinalLayer (last layers) target

updateNet :: Network -> [BackpropagatedLayer] -> Network
updateNet net [] = Network
  {
    layers = [],
    learningRate = learningRate net
  }
updateNet net (bl:bls) = Network
  {
    layers = update (learningRate net) bl  : layers (updateNet net bls),
    learningRate = learningRate net
  }

getWeights :: [PropagatedLayer] -> ColumnVector Double
getWeights = pW . last

getOutput :: [PropagatedLayer] -> ColumnVector Double
getOutput = pOut . last
