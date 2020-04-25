{-# LANGUAGE TemplateHaskell #-}
module NeuralNet.Network where

import NeuralNet.Model
import Control.Monad.State
import NeuralNet.LossFunction
import NeuralNet.DerivativeMath
import Control.Lens
import qualified Data.Map as M
import NeuralNet.Optimizer
import Debug.Trace (trace)

data Network = Network {
  _model :: !Model,
  _currentParams :: ![Double],
  _loss :: !LossFunction
}

makeLenses ''Network

createNetwork :: Model -> LossFunction -> [Double] -> Network
createNetwork model lossFunction initialParams =
  Network model initialParams' lossFunction
  where
    initialParams' = take (totalParams model) initialParams

trainingStep :: [Double] -> [Double] -> State Network Double
trainingStep ins outs = do
  network <- get
  let
    outputVars = map CVar [0 .. length outs - 1]
    l = (network ^. loss) outs outputVars
    outputDerivatives = map (`deriveTo` l) outputVars
    (modelOutputs, paramDerivatives) = outputsAndDerivativesModel (network ^. model) ins (network ^. currentParams) outputDerivatives
    currentLoss = runCalculation modelOutputs l
  zoom currentParams $ sgd 0.03 currentLoss paramDerivatives
  return currentLoss

predict :: Network -> [Double] -> [Double]
predict network ins =
  runModel (network ^. model) ins (network ^. currentParams)