module Lib
( ColumnVector(..)
, BackpropNet(..)
, ActivationSpec(..)
, identityAS
, tanhAS
, sigmoidAS
, Layer(..)
, PropagatedLayer(..)
, BackpropagatedLayer(..)
, propagate
, propagateNet
, backpropagate
, backpropagateFinalLayer
, backpropagateNet
, buildBackpropNet
, update
, updateNet
, getOutput
, getWeights
, getLayerWeights
, getErrGrad
, getErrGradLast
) where

import Data.Matrix

type ColumnVector a = Matrix a

data ActivationSpec = ActivationSpec
  {
    asF :: Double -> Double,
    asF' :: Double -> Double,
    desc :: String
  }

identityAS :: ActivationSpec
identityAS = ActivationSpec
  {
    asF = id,
    asF' = const 1,
    desc = "identity"
  }

tanhAS :: ActivationSpec
tanhAS = ActivationSpec
  {
    asF = tanh,
    asF' = tanh',
    desc = "tanh"
  }
  where tanh' x = 1 - tanh x ^ 2

sigmoidAS :: ActivationSpec
sigmoidAS = ActivationSpec
  {
    asF = sigmoid,
    asF' = sigmoid',
    desc = "sigmoid"
  }
  where sigmoid x = 1 / (1 + exp (-x))
        sigmoid' x = sigmoid x * (1 - sigmoid x)

data Layer = Layer
  {
    lW :: Matrix Double,
    lAS :: ActivationSpec
  }

data BackpropNet = BackpropNet
  {
    layers :: [Layer],
    learningRate :: Double
  }

checkDimensions :: Matrix Double -> Matrix Double -> Matrix Double
checkDimensions w1 w2 =
  if nrows w1 == ncols w2
    then w2
    else error "Inconsistent dimensions in weight matrix"

validateInput :: BackpropNet -> ColumnVector Double -> ColumnVector Double
validateInput bp input =
  if ncols firstlayer == nrows input
    then if withinRange (toList input)
      then input
      else error "Values in input matrix not within range (0,1)"
    else error ("Inconsistent dimensions in input matrix " ++ show (nrows firstlayer))
  where firstlayer = lW (head (layers bp))
        withinRange i = and (scanl inRange (firstInRange i) i)
        firstInRange a = head a >= 0 && head a <= 1
        inRange a b = a && (b >= 0 && b <= 1)

buildBackpropNet ::
  Double -> [Matrix Double] -> ActivationSpec -> BackpropNet
buildBackpropNet lr ws s = BackpropNet { layers=ls, learningRate=lr }
  where checkedWeights = scanl1 checkDimensions ws
        ls = map buildLayer checkedWeights
        buildLayer w = Layer { lW=w, lAS=s }

data PropagatedLayer
  = PropagatedLayer
    {
      -- The input to this layer
      pIn :: ColumnVector Double,
      -- The output from this layer
      pOut :: ColumnVector Double,
      -- The value of the first derivative of the activation function
      -- for this layer
      pF'a :: ColumnVector Double,
      -- The weights for this layers
      pW :: Matrix Double,
      -- The activation specification for this layer
      pAS :: ActivationSpec
    }
  | PropagatedSensorLayer
    {
      -- The output from this layer
      pOut :: ColumnVector Double
    }

propagate :: PropagatedLayer -> Layer -> PropagatedLayer
propagate layerJ layerK = PropagatedLayer
  {
    pIn = x,
    pOut = y,
    pF'a = f'a,
    pW = w,
    pAS = lAS layerK
  }
  where x = pOut layerJ
        w = lW layerK
        a = w * x
        f = asF (lAS layerK)
        y = fmap f a
        f' = asF' (lAS layerK)
        f'a = fmap f' a

propagateNet :: ColumnVector Double -> BackpropNet -> [PropagatedLayer]
propagateNet input net = tail calcs
  where calcs = scanl propagate layer0 (layers net)
        layer0 = PropagatedSensorLayer{ pOut=validatedInputs }
        validatedInputs = validateInput net input

data BackpropagatedLayer
  = BackpropagatedLayer
    {
      -- Del-sub-z-sub-l of E
      bpDazzle :: ColumnVector Double,
      -- The error due to this layer
      bpErrGrad :: ColumnVector Double,
      -- The value of the first derivative of the activation function
      -- for this layer
      bpF'a :: ColumnVector Double,
      -- The input to this layer
      bpIn :: ColumnVector Double,
      -- The output from this layer
      bpOut :: ColumnVector Double,
      -- The weights for this layers
      bpW :: Matrix Double,
      -- The activation specification for this layer
      bpAS :: ActivationSpec
    }

backpropagate ::
  PropagatedLayer -> BackpropagatedLayer -> BackpropagatedLayer
backpropagate layerJ layerK = BackpropagatedLayer
  {
    bpDazzle = dazzleJ,
    bpErrGrad = errorGrad dazzleJ f'aJ (pIn layerJ),
    bpF'a = pF'a layerJ,
    bpIn = pIn layerJ,
    bpOut = pOut layerJ,
    bpW = pW layerJ,
    bpAS = pAS layerJ
  }
  where dazzleJ = wKT * hadamard dazzleK f'aK
        dazzleK = bpDazzle layerK
        wKT = transpose (bpW layerK)
        f'aK = bpF'a layerK
        f'aJ = pF'a layerJ

errorGrad :: ColumnVector Double -> ColumnVector Double ->
             ColumnVector Double -> Matrix Double
errorGrad dazzle f'a input = hadamard dazzle f'a * transpose input

backpropagateFinalLayer ::
  PropagatedLayer -> ColumnVector Double -> BackpropagatedLayer
backpropagateFinalLayer l t = BackpropagatedLayer
  {
    bpDazzle = dazzle,
    bpErrGrad = errorGrad dazzle f'a (pIn l),
    bpF'a = pF'a l,
    bpIn = pIn l,
    bpOut = pOut l,
    bpW = pW l,
    bpAS = pAS l
  }
  where dazzle = pOut l - t
        f'a = pF'a l

backpropagateNet ::
  ColumnVector Double -> [PropagatedLayer] -> [BackpropagatedLayer]
backpropagateNet target layers = scanr backpropagate layerL hiddenLayers
  where hiddenLayers = init layers
        layerL = backpropagateFinalLayer (last layers) target

update :: Double -> BackpropagatedLayer -> Layer
update rate layer = Layer { lW = wNew, lAS = bpAS layer }
  where wOld = bpW layer
        delW = scaleMatrix rate (bpErrGrad layer)
        wNew = wOld - delW

updateNet :: BackpropNet -> [BackpropagatedLayer] -> BackpropNet
updateNet bpn [] = BackpropNet
  {
    layers = [],
    learningRate = learningRate bpn
  }
updateNet bpn (bl:bls) = BackpropNet
  {
    layers = update (learningRate bpn) bl  : layers (updateNet bpn bls),
    learningRate = learningRate bpn
  }

hadamard :: Matrix Double -> Matrix Double -> Matrix Double
hadamard m1 m2 = if m1c == m2c && m1r == m2r
  then fromList m1r m1c (zipWith (*) (toList m1) (toList m2))
  else error
    "Hadamard product is undefined for matrices with different dimensions"
  where m1r = nrows m1
        m1c = ncols m1
        m2r = nrows m2
        m2c = ncols m2

getWeights :: [PropagatedLayer] -> ColumnVector Double
getWeights = pW . last

getOutput :: [PropagatedLayer] -> ColumnVector Double
getOutput = pOut . last

getErrGradLast :: [BackpropagatedLayer] -> ColumnVector Double
getErrGradLast = bpErrGrad . last

getErrGrad :: [BackpropagatedLayer] -> ColumnVector Double
getErrGrad = bpErrGrad . last . init

getLayerWeights :: BackpropNet -> [Matrix Double]
getLayerWeights bpn = ws
  where ls = layers bpn
        ws = map lW ls
