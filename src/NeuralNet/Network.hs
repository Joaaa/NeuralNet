{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module NeuralNet.Network where

import NeuralNet.Model
import Control.Monad.State
import NeuralNet.LossFunction
import NeuralNet.DerivativeMath
import Control.Lens
import NeuralNet.Optimizer
import Debug.Trace (trace)
import qualified Data.Vector as V

data Network optimizer = Network {
  _model :: !Model,
  _currentParams :: !(V.Vector Double),
  _loss :: !LossFunction,
  _optimizer :: !optimizer
}

makeLenses ''Network

createNetwork :: Model -> LossFunction -> optimizer -> [Double] -> Network optimizer
createNetwork model lossFunction optimizer initialParams =
  Network model initialParams' lossFunction optimizer
  where
    initialParams' = V.fromList $ take (totalParams model) initialParams

trainingStep :: Optimizer optimizer => V.Vector Double -> V.Vector Double -> State (Network optimizer) Double
trainingStep ins outs = do
  network <- get
  let
    outputVars = map CVar [0 .. length outs - 1]
    l = (network ^. loss) (V.toList outs) outputVars
    outputDerivatives = map (`deriveTo` l) outputVars
    (modelOutputs, paramDerivatives) = outputsAndDerivativesModel (network ^. model) ins (network ^. currentParams) outputDerivatives
    currentLoss = runCalculation modelOutputs l
  newParams <- zoom optimizer $ optimize currentLoss paramDerivatives (network ^. currentParams)
  currentParams .= newParams
  return currentLoss

predict :: Network optimizer -> V.Vector Double -> V.Vector Double
predict network ins =
  runModel (network ^. model) ins (network ^. currentParams)