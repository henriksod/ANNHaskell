module ANN.ActivationFunction
( ActivationSpec(..)
, identityAS
, tanhAS
, sigmoidAS
) where

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

rectifierAS :: ActivationSpec
rectifierAS = ActivationSpec
  {
    asF = max 0,
    asF' = one . max 0,
    desc = "ReLU"
  }
  where one x = if x > 0 then 1 else 0

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
