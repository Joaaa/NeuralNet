module NeuralNet.Layer where

import NeuralNet.Network

class LayerFactory a where
  createLayer :: a -> Layer

type Layer = Network -> Network

class LayerInstance l where


