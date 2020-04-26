{-# LANGUAGE TemplateHaskell #-}
module NeuralNet.Optimizer where

import NeuralNet.DerivativeMath
import Control.Monad.State
import qualified Data.Vector as V
import Control.Lens
import Data.Vector.Mutable (write)
import Debug.Trace (trace)

class Optimizer s where
  optimize :: Double -> V.Vector Double -> V.Vector Double -> State s (V.Vector Double)

newtype SGD = SGD {
  lr :: Double
}

instance Optimizer SGD where
  optimize loss derivatives currentValues = do
    lr <- gets lr
    return $ do
      (v, d) <- V.zip currentValues derivatives
      return $ v - lr * d

sgd :: Double -> SGD
sgd = SGD

data Adam = Adam {
  _a :: Double,
  _b1 :: Double,
  _b2 :: Double,
  _t :: Int,
  _m :: V.Vector Double,
  _v :: V.Vector Double
} deriving Show

makeLenses ''Adam

instance Optimizer Adam where
  optimize loss derivatives currentValues = do
    a <- use a
    b1 <- use b1
    b2 <- use b2
    t += 1
    m1 <- m <%= \m0 -> V.fromList [b1 * m0 V.! i + (1 - b1) * derivatives V.! i | i <- [0 .. length m0 - 1]]
    v1 <- v <%= \v0 -> V.fromList [b2 * v0 V.! i + (1 - b2) * (derivatives V.! i) ^ 2 | i <- [0 .. length v0 - 1]]
    t <- use t
    return $ V.fromList [(currentValues V.! i) - a * ((m1 V.! i) / (1.0 - b1 ^ t)) / (sqrt ((v1 V.! i) / (1.0 - b2 ^ t)) + 1E-8) | i <- [0 .. length currentValues - 1]]

adam :: Int -> Double -> Double -> Double -> Adam
adam numParams stepSize b1 b2 = Adam stepSize b1 b2 0 (V.replicate numParams 0) (V.replicate numParams 0)
