module NeuralNet.LossFunction where

import NeuralNet.DerivativeMath

type LossFunction = [Double] -> [Calculation] -> Calculation

mseLoss :: LossFunction
mseLoss expected actual = sum (map (flip CPow 2) $ zipWith (\a b -> CConst a - b) expected actual) / fromIntegral (length actual)

categoricalCrossEntropy :: LossFunction
categoricalCrossEntropy expected actual = negate $ sum [CConst y * CLog py | (y, py) <- zip expected actual]

binaryCrossEntropy :: LossFunction
binaryCrossEntropy expected actual = (/ CConst (fromIntegral $ negate $ length expected)) $ sum [CConst y * CLog py + CConst (1 - y) * CLog (1 - py) | (y, py) <- zip expected actual]