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
  _model :: Model,
  _currentParams :: [Float],
  _loss :: Calculation,
  _expected :: [String],
  _partialDerivatives :: [Calculation]
} deriving Show

makeLenses ''Network

createNetwork :: Model -> LossFunction -> [Float] -> Network
createNetwork model lossFunction initialParams =
  Network model initialParams' loss expected partialDerivatives
  where
    initialParams' = take (length $ model ^. parameters) initialParams
    expected = ["o" <> show i | i <- [0 .. length (model ^. outputs) - 1]]
    loss = lossFunction (model ^. outputs) $ map CVar expected
    partialDerivatives = [deriveTo (CVar p) loss | p <- model ^. parameters]

trainingStep :: [Float] -> [Float] -> State Network Float
trainingStep ins outs = do
  network <- get
  let
    ce = MBCE $ M.fromList (zip (network ^. model . inputs) ins)
      <> M.fromList (zip (network ^. model . parameters) (network ^. currentParams))
      <> M.fromList (zip (network ^. expected) outs)
    currentLoss = runCalculation ce (network ^. loss)
    currentDerivatives = map (runCalculation ce) (network ^. partialDerivatives)
  zoom currentParams $ sgd 0.1 currentLoss currentDerivatives
  return currentLoss

predict :: Network -> [Float] -> [Float]
predict network ins =
  let ce = MBCE $ M.fromList (zip (network ^. model . inputs) ins)
        <> M.fromList (zip (network ^. model . parameters) (network ^. currentParams)) in
  map (runCalculation ce) (network ^. model . outputs)