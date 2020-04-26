module Main where

import qualified NeuralNet.Test
import qualified MNIST.MNIST

import System.Environment

main :: IO ()
main = getArgs >>= runProgram . head

runProgram "Testing" = NeuralNet.Test.main
runProgram "MNIST" = MNIST.MNIST.main