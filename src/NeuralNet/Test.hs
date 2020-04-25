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

run = main

main :: IO ()
main = do
  putStrLn "Starting"
  let model = sampleModel
  n <- createNetwork model mseLoss <$> replicateM (totalParams model) (randomRIO (-1.0, 1.0))
  as :: [Double] <- replicateM 100 $ randomRIO (-1.0, 1.0)
  bs :: [Double] <- replicateM 100 $ randomRIO (-1.0, 1.0)
  let ins = zipWith (\a b -> [a, b]) as bs
  let outs = zipWith (\a b -> [a + b]) as bs
  (losses, n') <- runStateT (forM [1..100] (\i -> liftIO (putStrLn $ "Iteration " <> show i) >> iteration ins outs)) n
  onscreen $ plot [1..length losses] losses % mp # "plot.yscale(\"log\")" % title "Loss"
  pred n' [0.5, -0.2]
  pred n' [0, 0]
  pred n' [0.1, 0.1]
  pred n' [0.3, 0.4]
  pred n' [0.8, -0.9]
  pred n' [0.5, 1]
  pred n' [2, 2]
--  forever $ do
--    hFlush stdout
--    (a, b) <- readLn :: IO (Double, Double)
--    pred n' [a, b]
  where pred n ins = putStrLn $ "f(" <> show ins <> ") = " <> show (predict n ins)

iteration :: [[Double]] -> [[Double]] -> StateT Network IO Double
iteration ins outs = do
  n <- get
  let (losses, n') = runState (forM (zip ins outs) (uncurry trainingStep)) n
  let avgLoss = sum losses / fromIntegral (length ins)
  liftIO $ do
    putStrLn $ "Average loss: " <> show avgLoss
    putStrLn $ "Parameter range: [" <> show (minimum $ n' ^. currentParams) <> ", " <> show (maximum $ n' ^. currentParams) <> "]"
  put n'
  return avgLoss

sampleModel = createModel 2 [
    dense 5,
    relu,
    dense 1
  ]
--
--sampleCalc = do
--  let x = CVar "x"
--  let y = CVar "y"
--  let der = deriveTo x (x*x*y)
--  print der
--  let ce = MBCE $ M.fromList [("x", 8), ("y", 3)]
--  print $ runCalculation ce der
--  print sampleModel
--  print $ map (deriveTo (CVar "x0")) $ sampleModel ^. outputs
