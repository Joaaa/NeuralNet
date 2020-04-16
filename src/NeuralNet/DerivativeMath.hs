{-# LANGUAGE DeriveDataTypeable #-}
module NeuralNet.DerivativeMath where

import Data.Data
import Control.Lens

data Calculation = DSum Calculation Calculation
  | DProd Calculation Calculation
  | DAbs Calculation
  | DSignum Calculation
  | DConst Float
  | DVar String
  deriving (Typeable, Data, Eq)

instance Plated Calculation

cSum (DConst 0) r = r
cSum l (DConst 0) = l
cSum (DConst l) (DConst r) = DConst $ l + r
cSum l r = DSum l r

cProd (DConst 0) r = DConst 0
cProd l (DConst 0) = DConst 0
cProd (DConst 1) r = r
cProd l (DConst 1) = l
cProd (DConst l) (DConst r) = DConst $ l * r
cProd (DSum sl sr) r = cSum (cProd sl r) (cProd sr r)
cProd l (DSum sl sr) = cSum (cProd l sl) (cProd l sr)
cProd l r = DProd l r

instance Num Calculation where
  l + r = DSum l r
  l * r = DProd l r
  abs = DAbs
  signum = DSignum
  fromInteger = DConst . fromInteger
  negate a = DProd a (fromInteger (-1))

deriveTo :: Calculation -> Calculation -> Calculation
deriveTo x (DSum l r) = cSum (deriveTo x l) (deriveTo x r)
deriveTo x (DProd l r) = cSum (cProd (deriveTo x l) r) (cProd l (deriveTo x r))
deriveTo x (DAbs a) = cProd (DSignum a) (deriveTo x a)
deriveTo x (DSignum _) = 0
deriveTo x (DConst _) = 0
deriveTo (DVar x) (DVar y)
  | y == x = 1
  | otherwise = 0

instance Show Calculation where
  show (DSum a b) = show a <> " + " <> show b
  show (DProd a b) = show a <> " * " <> show b
  show (DAbs a) = "abs(" <> show a <> ")"
  show (DSignum a) = "signum(" <> show a <> ")"
  show (DConst c) = show c
  show (DVar n) = n