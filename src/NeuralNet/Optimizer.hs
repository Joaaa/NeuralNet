{-# LANGUAGE RankNTypes #-}
module NeuralNet.Optimizer where

import NeuralNet.DerivativeMath
import qualified Data.Map as M
import Control.Monad.State

type Optimizer = Double -> [Double] -> State [Double] ()

sgd :: Double -> Optimizer
sgd lr loss derivatives = modify $ \oldValues -> [v - lr * d | (v, d) <- zip oldValues derivatives]
