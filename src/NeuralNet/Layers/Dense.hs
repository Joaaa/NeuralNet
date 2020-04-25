module NeuralNet.Layers.Dense where

import NeuralNet.Model
import Control.Lens
import Control.Monad
import NeuralNet.DerivativeMath
import Control.Monad.State (State)
import Data.Sequence (Seq)

dense :: Int -> LayerCreator
dense numHidden inputs =
  replicateM numHidden $ do
    params <- createParamVariables (length inputs + 1)
    return $ sum $ zipWith (*) (inputs <> [1]) params
