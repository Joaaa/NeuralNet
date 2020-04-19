{-# LANGUAGE DeriveDataTypeable #-}
module NeuralNet.DerivativeMath where

import Data.Data
import Control.Lens
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Monad.Reader

data Calculation = CSum Calculation Calculation
  | CProd Calculation Calculation
  | CPow Calculation Float
  | CAbs Calculation
  | CSignum Calculation
  | CVar String
  | CConst Float
  deriving (Typeable, Eq)

class CalculationEnvironment p where
  getVar :: p -> String -> Float

newtype MapBackedCalculationEnvironment = MBCE (M.Map String Float)

instance CalculationEnvironment MapBackedCalculationEnvironment where
  getVar (MBCE map) v = fromMaybe 0 (M.lookup v map)

runCalculation :: CalculationEnvironment e => e -> Calculation -> Float
runCalculation e (CSum l r) = runCalculation e l + runCalculation e r
runCalculation e (CProd l r) = runCalculation e l * runCalculation e r
runCalculation e (CPow l r) = runCalculation e l ** r
runCalculation e (CAbs a) = abs $ runCalculation e a
runCalculation e (CSignum a) = signum $ runCalculation e a
runCalculation e (CVar v) = getVar e v
runCalculation e (CConst c) = c

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
  show (CVar n) = n

relu x = (signum x + 1) * 0.5 * x