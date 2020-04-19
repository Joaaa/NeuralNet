{-# LANGUAGE DeriveDataTypeable #-}
module NeuralNet.DerivativeMath where

import Data.Data
import Control.Lens

data Calculation = CSum Calculation Calculation
  | CProd Calculation Calculation
  | CAbs Calculation
  | CSignum Calculation
  | CVar String
  | CConst Float
  deriving (Typeable, Data, Eq)

instance Plated Calculation

instance Num Calculation where
  l + r = CSum l r
  l * r = CProd l r
  abs = CAbs
  signum = CSignum
  fromInteger = CConst . fromInteger
  negate a = CProd a (fromInteger (-1))

deriveTo :: Calculation -> Calculation -> Calculation
deriveTo x (CSum l r) = CSum (deriveTo x l) (deriveTo x r)
deriveTo x (CProd l r) = CSum (CProd (deriveTo x l) r) (CProd l (deriveTo x r))
deriveTo x (CAbs a) = CProd (CSignum a) (deriveTo x a)
deriveTo x (CSignum _) = 0
deriveTo x (CConst _) = 0
deriveTo (CVar x) (CVar y)
  | y == x = 1
  | otherwise = 0

instance Show Calculation where
  show (CSum a b) = "(" <> show a <> " + " <> show b <> ")"
  show (CProd a b) = "(" <> show a <> " * " <> show b <> ")"
  show (CAbs a) = "abs(" <> show a <> ")"
  show (CSignum a) = "signum(" <> show a <> ")"
  show (CConst c) = show c
  show (CVar n) = n