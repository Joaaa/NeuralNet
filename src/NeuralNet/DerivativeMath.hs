{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module NeuralNet.DerivativeMath where

import Data.Data
import Control.Lens
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import qualified Data.Vector as V
import qualified Data.Sequence as S
import Data.Foldable (fold)

data Calculation = CSum !Calculation !Calculation
  | CProd !Calculation !Calculation
  | CPow !Calculation !Double
  | CAbs !Calculation
  | CSignum !Calculation
  | CVar !Int
  | CConst !Double
  deriving (Typeable, Eq)

class Monoid p => CalculationEnvironment p where
  varAt :: p -> Int -> Double

instance CalculationEnvironment [Double] where
  varAt = (!!)

instance CalculationEnvironment (V.Vector Double) where
  varAt = (V.!)

instance CalculationEnvironment (S.Seq Double) where
  varAt = S.index

instance CalculationEnvironment () where
  varAt _ _ = 0

runCalculation :: CalculationEnvironment e => e -> Calculation -> Double
runCalculation e (CSum l r) = runCalculation e l + runCalculation e r
runCalculation e (CProd l r) = runCalculation e l * runCalculation e r
runCalculation e (CPow l r) = runCalculation e l ** r
runCalculation e (CAbs a) = abs $ runCalculation e a
runCalculation e (CSignum a) = signum $ runCalculation e a
runCalculation e (CVar v) = varAt e v
runCalculation e (CConst c) = c

runCalculations :: (Foldable f, CalculationEnvironment e, Traversable t) => f e -> t Calculation -> t Double
runCalculations environments = fmap (runCalculation (fold environments))

instance Num Calculation where
  0 + r = r
  r + 0 = r
  (CConst l) + (CConst r) = CConst (l + r)
  l + r = CSum l r
  0 * r = 0
  r * 0 = 0
  1 * r = r
  r * 1 = r
  (CConst l) * (CConst r) = CConst (l * r)
  l * r = CProd l r
  abs (CConst a) = CConst (abs a)
  abs a = CAbs a
  signum (CConst a) = CConst (signum a)
  signum a = CSignum a
  fromInteger = CConst . fromInteger
  negate (CConst a) = CConst (negate a)
  negate a = CProd a (fromInteger (-1))

instance Fractional Calculation where
  fromRational = CConst . fromRational
  a / (CConst c) = a * CConst (1 / c)

deriveTo :: Calculation -> Calculation -> Calculation
deriveTo x (CSum l r) = deriveTo x l + deriveTo x r
deriveTo x (CProd l r) = deriveTo x l * r + l * deriveTo x r
deriveTo x (CPow l r) = CConst r * CPow l (r - 1) * deriveTo x l
deriveTo x (CAbs a) = signum a * deriveTo x a
deriveTo x (CSignum _) = 0
deriveTo x (CConst _) = 0
deriveTo (CVar x) (CVar y)
  | y == x = 1
  | otherwise = 0

instance Show Calculation where
  show (CSum a b) = "(" <> show a <> " + " <> show b <> ")"
  show (CProd a b) = "(" <> show a <> " * " <> show b <> ")"
  show (CPow a b) = "(" <> show a <> " ** " <> show b <> ")"
  show (CAbs a) = "abs(" <> show a <> ")"
  show (CSignum a) = "signum(" <> show a <> ")"
  show (CConst c) = show c
  show (CVar n) = "[" <> show n <> "]"
