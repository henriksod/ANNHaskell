module ANN.Layer
( ColumnVector(..)
, Layer(..)
, PropagatedLayer(..)
, BackpropagatedLayer(..)
, propagate
, backpropagate
, backpropagateFinalLayer
, update
) where

import ANN.ActivationFunction
import ANN.MatrixUtils
import Data.Matrix

data Layer = Layer
  {
    lW :: Matrix Double,
    lB :: ColumnVector Double,
    lAS :: ActivationSpec
  }

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
      -- The weights for this layer
      pW :: Matrix Double,
      -- The biases for this layer
      pB :: ColumnVector Double,
      -- The activation specification for this layer
      pAS :: ActivationSpec
    }
  | PropagatedSensorLayer
    {
      -- The output from this layer
      pOut :: ColumnVector Double
    }

data BackpropagatedLayer
  = BackpropagatedLayer
    {
      -- Del-sub-z-sub-l of E
      bpDazzle :: ColumnVector Double,
      -- The error due to this layer
      bpErrGrad :: ColumnVector Double,
      -- The bias error due to this layer
      bpBiasErrGrad :: ColumnVector Double,
      -- The value of the first derivative of the activation function
      -- for this layer
      bpF'a :: ColumnVector Double,
      -- The input to this layer
      bpIn :: ColumnVector Double,
      -- The output from this layer
      bpOut :: ColumnVector Double,
      -- The weights for this layer
      bpW :: Matrix Double,
      -- The biases for this layer
      bpB :: ColumnVector Double,
      -- The activation specification for this layer
      bpAS :: ActivationSpec
    }

propagate :: PropagatedLayer -> Layer -> PropagatedLayer
propagate layerJ layerK = PropagatedLayer
  {
    pIn = x,
    pOut = y,
    pF'a = f'a,
    pW = w,
    pB = b,
    pAS = lAS layerK
  }
  where x = pOut layerJ
        w = lW layerK
        b = lB layerK
        a = w * x + b
        f = asF (lAS layerK)
        y = fmap f a
        f' = asF' (lAS layerK)
        f'a = fmap f' a

backpropagate ::
  PropagatedLayer -> BackpropagatedLayer -> BackpropagatedLayer
backpropagate layerJ layerK = BackpropagatedLayer
  {
    bpDazzle = dazzleJ,
    bpErrGrad = errorGrad dazzleJ f'aJ (pIn layerJ),
    bpBiasErrGrad = biasErrorGrad dazzleJ f'aJ,
    bpF'a = pF'a layerJ,
    bpIn = pIn layerJ,
    bpOut = pOut layerJ,
    bpW = pW layerJ,
    bpB = pB layerJ,
    bpAS = pAS layerJ
  }
  where dazzleJ = wKT * hadamard dazzleK f'aK
        dazzleK = bpDazzle layerK
        wKT = transpose (bpW layerK)
        f'aK = bpF'a layerK
        f'aJ = pF'a layerJ

backpropagateFinalLayer ::
  PropagatedLayer -> ColumnVector Double -> BackpropagatedLayer
backpropagateFinalLayer l t = BackpropagatedLayer
  {
    bpDazzle = dazzle,
    bpErrGrad = errorGrad dazzle f'a (pIn l),
    bpBiasErrGrad = biasErrorGrad dazzle f'a,
    bpF'a = pF'a l,
    bpIn = pIn l,
    bpOut = pOut l,
    bpW = pW l,
    bpB = pB l,
    bpAS = pAS l
  }
  where dazzle = pOut l - t
        f'a = pF'a l

update :: Double -> BackpropagatedLayer -> Layer
update rate layer = Layer { lW = wNew, lB = bNew, lAS = bpAS layer }
  where wOld = bpW layer
        bOld = bpB layer
        delW = scaleMatrix rate (bpErrGrad layer)
        delB = scaleMatrix rate (bpBiasErrGrad layer)
        wNew = wOld - delW
        bNew = bOld - delB

errorGrad :: ColumnVector Double -> ColumnVector Double ->
             ColumnVector Double -> Matrix Double
errorGrad dazzle f'a input = hadamard dazzle f'a * transpose input

biasErrorGrad :: ColumnVector Double -> ColumnVector Double ->
                 ColumnVector Double
biasErrorGrad = hadamard
