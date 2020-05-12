# NeuralNet

A neural network implementation from scratch in Haskell.

## Instructions

`> Stack build`

Builds the executable

`> Stack run MNIST`

_Warning: Processor intensive_

Runs the MNIST program, which trains a very simple network for 300 iterations on the [MNIST](http://yann.lecun.com/exdb/mnist/) dataset.
Program will output training losses and test accuracy (expected 80%-90%).
Enter test image index into stdin to show prediction for that image.
