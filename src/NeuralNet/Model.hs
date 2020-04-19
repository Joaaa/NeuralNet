{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
module NeuralNet.Model where

import Control.Monad.State
import Control.Lens
import NeuralNet.DerivativeMath
import Debug.Trace (trace)
import qualified Data.Map as M

class LayerDescriptor l where
  showLayer :: l -> String
  addToModel :: l -> State Model ()

data Layer = forall l. LayerDescriptor l => Layer {
  _descriptor :: l
}

instance Show Layer where
  show (Layer l) = showLayer l

data Model = Model {
  _inputs :: [String],
  _outputs :: [Calculation],
  _parameters :: [String],
  _layers :: [Layer]
} deriving Show

makeLenses ''Layer
makeLenses ''Model

createModel :: Int -> Model
createModel numInputs =
  let inputs = ["i" <> show i | i <- [0..(numInputs-1)]] in
  Model inputs (map CVar inputs) [] []

createParam :: State Model String
createParam = do
  l <- length <$> use parameters
  let name = "x" ++ show l
  parameters <>= [name]
  return name

createParams :: Int -> State Model [String]
createParams = flip replicateM createParam

runModel :: Model -> [Float] -> [Float] -> [Float]
runModel model ins params =
  let ce = MBCE $ M.fromList (zip (_inputs model) ins) <> M.fromList (zip (_parameters model) params) in
  map (runCalculation ce) (_outputs model)
