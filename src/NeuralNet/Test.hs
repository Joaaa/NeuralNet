module NeuralNet.Test where

import Data.Monoid (Sum)
import NeuralNet.DerivativeMath
import Control.Lens

main :: IO ()
main = do
  putStrLn "Test"
  let x = DVar "x"
  let y = DVar "y"
  let expr = 5 * x + 3 * (x * y) + 7 + (5 * x * x * x)
  let der = deriveTo x expr
  print expr
  print $ expr & plate .~ DConst 8
  print $ universe expr
  print $ transform add1 expr
  print $ transform simplify expr
  print der
  print $ transform simplify der

add1 (DConst i) = DConst $ i+1
add1 a = a

simplify (DSum l r) | l == r = cProd 2 l
simplify a = a