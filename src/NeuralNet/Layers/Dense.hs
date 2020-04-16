module NeuralNet.Layers.Dense where

import NeuralNet.Layer

data Dense = Dense {
  numNodes :: Int
}

data DenseLayerInstance = DenseLayerInstance {

}

instance LayerFactory Dense where
  createLayer Dense{numNodes = n} = undefined