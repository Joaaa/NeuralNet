module NeuralNet.Test where

import Data.Monoid (Sum)
import NeuralNet.DerivativeMath
import Control.Lens
import Util

main :: IO ()
main = do
  putStrLn "Test"
  let x = CVar "x"
  let y = CVar "y"
  let expr = 5 * x + 3 * (x * y) + 7 + (5 * x * x * x)
  let der = deriveTo x expr
  print expr
  print $ transform simplify expr
  putStrLn "---"
  print der
  print $ nTimes 100 (transform simplify) der


simplify :: Calculation -> Calculation
simplify (CSum l r) | shouldSwitch l r = CSum r l
simplify (CProd l r) | shouldSwitch l r = CProd r l

simplify (CSum (CConst l) (CConst r)) = CConst (l + r)
simplify (CProd (CConst l) (CConst r)) = CConst (l * r)

simplify (CSum (CConst 0) r) = r
simplify (CProd (CConst 0) r) = CConst 0
simplify (CProd (CConst 1) r) = r

simplify (CSum (CSum ll lr) r) = ll + (lr + r)
simplify (CProd (CProd ll lr) r) = ll * (lr * r)

simplify (CSum (CProd (CConst l1) r1) (CProd (CConst l2) r2)) | r1 == r2 = (r1 + r2) * r1
simplify (CSum r1 (CProd (CConst l2) r2)) | r1 == r2 = (1 + r2) * r1
simplify (CSum (CProd (CConst l1) r1) r2) | r1 == r2 = (r1 + 1) * r1

simplify (CProd (CSum ll lr) r) = ll * r + lr * r
simplify (CProd l (CSum rl rr)) = l * rl + l * rr

simplify a = a

shouldSwitch :: Calculation -> Calculation -> Bool
shouldSwitch l (CConst _) = case l of
  (CConst _) -> False
  _ -> True
shouldSwitch _ _ = False