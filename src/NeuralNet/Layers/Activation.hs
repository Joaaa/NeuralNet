module NeuralNet.Layers.Activation where

import NeuralNet.Model
import Control.Lens
import NeuralNet.DerivativeMath

relu :: LayerCreator
relu = return . map relu'
  where
    relu' x = (signum x + 1) * 0.5 * x
