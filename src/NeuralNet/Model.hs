{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BlockArguments #-}
module NeuralNet.Model where

import Control.Monad.State
import Control.Lens
import NeuralNet.DerivativeMath
import qualified Data.Vector as V

type Input = Calculation
type Parameter = Calculation
type OutputDerivative = Calculation

newtype LayerCreatorState = LayerCreatorState {
  _currentParamIndex :: Int
} deriving Show

type LayerCreator = [Input] -> State LayerCreatorState [OutputDerivative]

data CompiledLayer = CompiledLayer {
  _numInputs :: !Int,
  _numParams :: !Int,
  _numOutputs :: !Int,
  _outputs :: ![Calculation],
  _inputDerivatives :: ![Calculation],
  _paramDerivatives :: ![Calculation]
} deriving Show

newtype Model = Model {
  _layers :: [CompiledLayer]
} deriving Show

makeLenses ''LayerCreatorState
makeLenses ''CompiledLayer
makeLenses ''Model

totalParams :: Model -> Int
totalParams = sum . map _numParams . _layers

createParamVariables :: Int -> State LayerCreatorState [Parameter]
createParamVariables n = replicateM n (CVar <$> (currentParamIndex <<+= 1))

compileLayer :: Int -> LayerCreator -> CompiledLayer
compileLayer numInputs creator =
  CompiledLayer numInputs numParams (length outputs) outputs inputDerivatives paramDerivatives
  where
    inputs = map CVar [0 .. numInputs - 1]
    (outputs, LayerCreatorState paramIndex) = runState (creator inputs) (LayerCreatorState numInputs)
    numParams = paramIndex - numInputs
    params = map CVar [numInputs .. paramIndex - 1]
    outputDerivativeVariables = map (CVar . (+ (numInputs + length params))) [0 .. length outputs - 1]
    inputDerivatives = map (\i -> sum $ zipWith (*) (map (deriveTo i) outputs) outputDerivativeVariables) inputs
    paramDerivatives = map (\p -> sum $ zipWith (*) (map (deriveTo p) outputs) outputDerivativeVariables) params

createModel :: Int -> [LayerCreator] -> Model
createModel numInputs layers =
  Model $ flip evalState numInputs $ forM layers $ \layer -> do
    currNumIn <- get
    let compiledLayer = compileLayer currNumIn layer
    put $ compiledLayer ^. numOutputs
    return compiledLayer

runModel :: Model -> V.Vector Double -> V.Vector Double -> V.Vector Double
runModel model ins params =
  fst $ flip execState (ins, params) $ forM_ (model ^. layers) $ \layer -> do
    (currIns, (currParams, nextParams)) <- gets $ over _2 (V.splitAt (layer ^. numParams))
    put (calculateLayerOutput layer currIns currParams, nextParams)

outputsAndDerivativesModel :: Model -> V.Vector Double -> V.Vector Double -> [Calculation] -> (V.Vector Double, V.Vector Double)
outputsAndDerivativesModel model ins params outputDerivatives =
  let (_, pd, o) = go (model ^. layers) ins params in (o, pd)
  where
    go :: [CompiledLayer] -> V.Vector Double -> V.Vector Double -> (V.Vector Double, V.Vector Double, V.Vector Double)
    go [] ins _ = (V.fromList $ runCalculations [ins] outputDerivatives, mempty, ins)
    go (lh:lt) ins params = (inputDerivatives, paramDerivatives <> nextParamDerivatives, finalOutputs)
      where
        (currParams, nextParams) = V.splitAt (lh ^. numParams) params
        outputs = calculateLayerOutput lh ins currParams
        (outputDerivatives, nextParamDerivatives, finalOutputs) = go lt outputs nextParams
        (inputDerivatives, paramDerivatives) = calculateDerivatives lh ins currParams outputDerivatives



calculateLayerOutput :: CompiledLayer -> V.Vector Double -> V.Vector Double -> V.Vector Double
calculateLayerOutput layer inputs params = V.fromList $ runCalculations [inputs, params] (layer ^. outputs)

calculateDerivatives :: CompiledLayer -> V.Vector Double -> V.Vector Double -> V.Vector Double -> (V.Vector Double, V.Vector Double)
calculateDerivatives layer ins params outputDerivatives = (layer ^. inputDerivatives, layer ^. paramDerivatives) & both %~ V.fromList . runCalculations [ins, params, outputDerivatives]

---- loss -> nextLayers -> inputs -> (outputDerivatives, allParamAccessor)
--test :: ([Calculation] -> [Calculation]) -> [CompiledLayer] -> [Double] -> ([Double], Traversal' [CompiledLayer] [Double])
--test loss [] finalOutputs = (undefined, empty)