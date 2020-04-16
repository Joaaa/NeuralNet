module NeuralNet.Network where

data Network = Network {
  numInputs :: Int,
  numOutputs :: Int,
  parameters :: [Float]
}