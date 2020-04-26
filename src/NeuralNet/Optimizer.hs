{-# LANGUAGE RankNTypes #-}
module NeuralNet.Optimizer where

import NeuralNet.DerivativeMath
import qualified Data.Map as M
import Control.Monad.State
import qualified Data.Vector as V

type Optimizer = Double -> V.Vector Double -> State (V.Vector Double) ()

sgd :: Double -> Optimizer
sgd lr loss derivatives = modify $ \oldValues -> do
  (v, d) <- V.zip oldValues derivatives
  return $ v - lr * d
