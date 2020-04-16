module NeuralNet.Tensor where

-- For now, tensors are just vectors
data Tensor a = Tensor {
  size :: Int,
  content :: [a]
}

fromList l = Tensor (length l) l