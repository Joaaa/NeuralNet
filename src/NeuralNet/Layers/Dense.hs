module NeuralNet.Layers.Dense where

import NeuralNet.Model
import Control.Lens
import Control.Monad
import NeuralNet.DerivativeMath

newtype Dense = Dense {
  numNodes :: Int
} deriving Show

instance LayerDescriptor Dense where
  showLayer = show
  addToModel l@(Dense numNodes) = do
    prevOutputs <- use outputs
    o <- replicateM numNodes $ do
      let nodeInputs = CConst 1 : prevOutputs
      params <- createParams $ length nodeInputs
      return $ sum $ zipWith (*) (map CVar params) nodeInputs
    outputs .= o
    layers <>= [Layer l]
