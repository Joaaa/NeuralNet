module NeuralNet.LossFunction where

import NeuralNet.DerivativeMath

type LossFunction = [Calculation] -> [Calculation] -> Calculation

mseLoss :: LossFunction
mseLoss expected actual = sum (map (flip CPow 2) $ zipWith (-) expected actual) / fromIntegral (length actual)
