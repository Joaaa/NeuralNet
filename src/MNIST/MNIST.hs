module MNIST.MNIST where

import qualified Data.ByteString as BS
import Control.Monad.State
import Control.Lens hiding ((#))
import Debug.Trace (trace)
import Graphics.Matplotlib
import NeuralNet.Model
import NeuralNet.Layers.Dense
import NeuralNet.Layers.Activation
import NeuralNet.LossFunction
import NeuralNet.Optimizer
import NeuralNet.Network
import System.Random
import qualified Data.Vector as V
import System.IO (hFlush, stdout)
import Text.Printf (printf)

run = main

main :: IO ()
main = do
  putStrLn "Starting."
  (numLabels, labels) <- readLabels "MNIST/train-labels"
  (numImages, numRows, numCols, images) <- readImages "MNIST/train-images"
  network <- initNetwork (numRows * numCols)
  evaluate network False
--  print $ predict network $ V.fromList $ map (\x -> fromIntegral x / 127.5 - 1) $ head images
  (losses, n') <- runStateT (forM [1..300] (\i -> liftIO (putStrLn $ "Iteration " <> show i) >> iteration images labels)) network
  onscreen $ plot [1..length losses] losses % mp # "plot.yscale(\"log\")" % title "Loss"
  evaluate n' True
  readLn

iteration :: Optimizer optimizer => V.Vector (V.Vector Int) -> V.Vector Int -> StateT (Network optimizer) IO Double
iteration images labels = do
  n <- get
  let batchSize = 100
  batch <- liftIO $ V.fromList <$> replicateM batchSize (randomRIO (0, length labels - 1))
  let ins = fmap (prepareImage . (images V.!)) batch
  let outs = fmap (prepareLabel . (labels V.!)) batch
  let (losses, n') = runState (forM (V.zip ins outs) (uncurry trainingStep)) n
  let avgLoss = sum losses / fromIntegral (length ins)
  liftIO $ do
    putStrLn $ "Average loss: " <> show avgLoss
    putStrLn $ "Parameter range: [" <> show (minimum $ n' ^. currentParams) <> ", " <> show (maximum $ n' ^. currentParams) <> "]"
    hFlush stdout
  put n'
  return avgLoss

evaluate :: Network optimizer -> Bool -> IO ()
evaluate network visualize = do
  (numLabels, labels) <- readLabels "MNIST/test-labels"
  (numImages, numRows, numCols, images) <- readImages "MNIST/test-images"
  let correct = sum $ map (\i -> if argmax (V.toList $ predict network (prepareImage $ images V.! i)) == labels V.! i then 1 else 0) [0 .. numLabels - 1] :: Int
  printf "Accuracy: %d/%d (%.3f)\n" correct numLabels ((100.0 * fromIntegral correct / fromIntegral numLabels) :: Float)
  hFlush stdout
  when visualize $ forever $ do
    i <- readLn :: IO Int
    drawImage ("Digit: " <> show (labels V.! i) <> ", predicted: " <> show (argmax (V.toList $ predict network (prepareImage $ images V.! i)))) numRows numCols (images V.! i)

argmax :: (Ord a) => [a] -> Int
argmax = snd . maximum . flip zip [0..]

prepareImage :: V.Vector Int -> V.Vector Double
prepareImage = fmap (\x -> fromIntegral x / 127.5 - 1)

prepareLabel :: Int -> V.Vector Double
prepareLabel l = V.fromList [if l == i then 1 else 0 | i <- [0..9]]

initNetwork :: Int -> IO (Network Adam)
initNetwork numInputs = do
  let model = createModel numInputs [dense 10, softmax]
  createNetwork model categoricalCrossEntropy (adam (totalParams model) 0.001 0.9 0.999) <$> replicateM (totalParams model) (randomRIO (-1.0, 1.0))

readLabels :: FilePath -> IO (Int, V.Vector Int)
readLabels fn = evalState parseContent <$> BS.readFile fn
  where
    parseContent :: State BS.ByteString (Int, V.Vector Int)
    parseContent = do
      modify $ BS.drop 4
      numLabels <- readInt32
      labels <- readBytes numLabels
      return (numLabels, V.fromList labels)

readImages :: FilePath -> IO (Int, Int, Int, V.Vector (V.Vector Int))
readImages fn = evalState parseContent <$> BS.readFile fn
  where
    parseContent :: State BS.ByteString (Int, Int, Int, V.Vector (V.Vector Int))
    parseContent = do
      modify $ BS.drop 4
      numImages <- readInt32
      numRows <- readInt32
      numCols <- readInt32
      images <- replicateM numImages $ readBytes (numRows * numCols)
      return (numImages, numRows, numCols, V.fromList $ map V.fromList images)

readInt32 :: State BS.ByteString Int
readInt32 = foldl (\a b -> a * 0x100 + fromIntegral b) 0 . BS.unpack <$> state (BS.splitAt 4)

readBytes :: Int -> State BS.ByteString [Int]
readBytes n = map fromIntegral . BS.unpack <$> state (BS.splitAt n)

drawImage :: String -> Int -> Int -> V.Vector Int -> IO ()
drawImage header rows cols img =
  let img' = reverse $ evalState (replicateM rows $ state $ splitAt cols) $ V.toList img in
    onscreen $ pcolor img' % title header
