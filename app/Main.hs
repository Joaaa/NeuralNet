module Main where

import qualified StoreComonad
import qualified NeuralNet.Test

import System.Environment

main :: IO ()
main = getArgs >>= runProgram . head

runProgram "StoreComonad" = StoreComonad.main
runProgram "NeuralNet" = NeuralNet.Test.main