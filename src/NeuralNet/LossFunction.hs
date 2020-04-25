module NeuralNet.LossFunction where

import NeuralNet.DerivativeMath

type LossFunction = [Double] -> [Calculation] -> Calculation

mseLoss :: LossFunction
mseLoss expected actual = sum (map (flip CPow 2) $ zipWith (\a b -> CConst a - b) expected actual) / fromIntegral (length actual)
