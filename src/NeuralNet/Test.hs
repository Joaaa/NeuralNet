{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NeuralNet.Test where

import Data.Monoid (Sum)
import NeuralNet.DerivativeMath
import Control.Lens hiding (preview)
import Util
import NeuralNet.Model
import NeuralNet.Network
import NeuralNet.LossFunction
import Control.Monad.State
import NeuralNet.Layers.Dense
import NeuralNet.Layers.Activation
import qualified Data.Map as M
import System.Random

main :: IO ()
main = do
  putStrLn "Starting"
  n <- createNetwork sampleModel mseLoss <$> replicateM 1000 (randomRIO (-1.0, 1.0))
  as :: [Float] <- replicateM 100 $ randomRIO (-1.0, 1.0)
  bs :: [Float] <- replicateM 100 $ randomRIO (-1.0, 1.0)
  let ins = zipWith (\a b -> [a, b]) as bs
  let outs = zipWith (\a b -> [a + b]) as bs
  print as
  print bs
  n' <- execStateT (forM_ [1..100] (\i -> liftIO (putStrLn $ "Iteration " <> show i) >> iteration ins outs)) n
  pred n' [0.5, -0.2]
  pred n' [0, 0]
  pred n' [0.1, 0.1]
  putStrLn "Done"
  where pred n ins = putStrLn $ "f(" <> show ins <> ") = " <> show (predict n ins)

iteration :: [[Float]] -> [[Float]] -> StateT Network IO ()
iteration ins outs = do
  n <- get
  let (losses, n') = runState (forM (zip ins outs) (uncurry trainingStep)) n
  liftIO $ do
    putStrLn $ "Average loss: " <> show (sum losses / fromIntegral (length ins))
    putStrLn $ "Losses: " <> show losses
    putStrLn $ "Parameters: " <> show (n' ^. currentParams)
  put n'

sampleModel = flip execState (createModel 2) $ do
  addToModel (Dense 3)
  addToModel (Activation "relu" relu)
  addToModel (Dense 1)

sampleCalc = do
  let x = CVar "x"
  let y = CVar "y"
  let der = deriveTo x (x*x*y)
  print der
  let ce = MBCE $ M.fromList [("x", 8), ("y", 3)]
  print $ runCalculation ce der
  print sampleModel
  print $ map (deriveTo (CVar "x0")) $ sampleModel ^. outputs
