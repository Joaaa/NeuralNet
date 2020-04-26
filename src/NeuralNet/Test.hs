{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module NeuralNet.Test where

import Data.Monoid (Sum)
import NeuralNet.DerivativeMath
import Control.Lens hiding (preview, (#))
import Util
import NeuralNet.Model
import NeuralNet.Network
import NeuralNet.LossFunction
import Control.Monad.State
import NeuralNet.Layers.Dense
import NeuralNet.Layers.Activation
import qualified Data.Map as M
import System.Random
import Graphics.Matplotlib
import System.IO (stdout, hFlush)
import qualified Data.Vector as V
import NeuralNet.Optimizer

main :: IO ()
main = do
  putStrLn "Starting"
  let model = sampleModel
  putStrLn $ "Trainable parameters: " <> show (totalParams model)
  n <- createNetwork model mseLoss (sgd 0.03) <$> replicateM (totalParams model) (randomRIO (-1.0, 1.0))
  as :: [Double] <- replicateM 100 $ randomRIO (-1.0, 1.0)
  bs :: [Double] <- replicateM 100 $ randomRIO (-1.0, 1.0)
  let ins = zipWith (\a b -> [a, b]) as bs
  let outs = zipWith (\a b -> [a * b]) as bs
  onscreen $ scatter (map (!!0) ins) (map (!!1) ins) @@ [o2 "c" (map (!!0) outs)] % title "Training data"
  (losses, n') <- runStateT (forM [1..1000] (\i -> liftIO (putStrLn $ "Iteration " <> show i) >> iteration ins outs)) n
  onscreen $ plot [1..length losses] losses % mp # "plot.yscale(\"log\")" % title "Loss"
  pred n' [0.5, -0.2]
  pred n' [0, 0]
  pred n' [0.1, 0.1]
  pred n' [0.3, 0.4]
  pred n' [0.8, -0.9]
  pred n' [0.5, 1]
  pred n' [2, 2]
  hFlush stdout
  readLn
  where pred n ins = putStrLn $ "f(" <> show ins <> ") = " <> show (predict n $ V.fromList ins)

iteration :: Optimizer optimizer => [[Double]] -> [[Double]] -> StateT (Network optimizer) IO Double
iteration ins outs = do
  n <- get
  let ins' = map V.fromList ins
  let outs' = map V.fromList outs
  let (losses, n') = runState (forM (zip ins' outs') (uncurry trainingStep)) n
  let avgLoss = sum losses / fromIntegral (length ins)
  liftIO $ do
    putStrLn $ "Average loss: " <> show avgLoss
    putStrLn $ "Parameter range: [" <> show (minimum $ n' ^. currentParams) <> ", " <> show (maximum $ n' ^. currentParams) <> "]"
    hFlush stdout
  put n'
  return avgLoss

sampleModel = createModel 2 [
    dense 5,
    relu,
    dense 3,
    relu,
    dense 1
  ]
