module NeuralNet.Layers.Activation where

import NeuralNet.Model
import Control.Lens
import NeuralNet.DerivativeMath

data Activation = Activation {
  activationName :: String,
  activationFunction :: Calculation -> Calculation
}

instance LayerDescriptor Activation where
  showLayer l = "Activation {type = " <> activationName l <> "}"
  addToModel l@(Activation _ f) = do
    prevOutputs <- use outputs
    outputs .= map f prevOutputs
    layers <>= [Layer l]