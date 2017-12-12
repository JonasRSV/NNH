module Main where

import System.Random

data Neuron = OpenNeuron Double | ConnectedNeuron Double Double [Double]
  deriving(Show)

data Network = ConnectedNetwork [Neuron] Network | OpenNetwork [Neuron]
  deriving(Show)

-- Accessors

netActivation :: Neuron -> Double
netActivation (OpenNeuron a) = a
netActivation (ConnectedNeuron a _ _) = a

neuralScalars :: Neuron -> [Double]
neuralScalars OpenNeuron{} = error "Open neurons has no connections"
neuralScalars (ConnectedNeuron _ _ a) = a

networkNeurons :: Network -> [Neuron]
networkNeurons (ConnectedNetwork a _) = a
networkNeurons (OpenNetwork a) = a

-- Updaters

updateNetActivation :: Neuron -> Double -> Neuron
updateNetActivation (OpenNeuron _) a = OpenNeuron a
updateNetActivation (ConnectedNeuron _ c d) a = ConnectedNeuron a c d

updateScalars :: Neuron -> [Double] -> Neuron
updateScalars OpenNeuron{} _ = error "Open neuron has no weights, thus cant update"
updateScalars (ConnectedNeuron a c _) d = ConnectedNeuron a c d

-- Generators

openNeuronGenerator :: [Neuron]
openNeuronGenerator = cycle [OpenNeuron 0.0]

connectedNeuronGenerator :: [[Double] -> Neuron]
connectedNeuronGenerator = cycle [ConnectedNeuron 0.0 0]

-- Network Creating

npi :: StdGen -> [Int] -> Network
npi seed (previous:current:next:rest) =
    let nextNetwork = npi seed (current:next:rest)
        connections = take previous $ randoms seed
        currentNeurons = map (\initializer -> initializer connections) (take current connectedNeuronGenerator)
    in ConnectedNetwork currentNeurons nextNetwork

npi seed [previous, current] =
    let connections = take previous $ randoms seed 
        currentNeurons = map (\initializer -> initializer connections) (take current connectedNeuronGenerator)
    in OpenNetwork currentNeurons


networkInitializer :: StdGen -> [Int] -> Network
networkInitializer seed schematic@(inputNodes:_) = ConnectedNetwork (take inputNodes openNeuronGenerator) (npi seed schematic)

-- Actual Functionalities

networkFmap :: (Neuron -> Neuron) -> Network -> Network
networkFmap f (ConnectedNetwork neurons network) = ConnectedNetwork (map f neurons) network
networkFmap f (OpenNetwork neurons) = OpenNetwork (map f neurons)


-- Neuron Activation

activateBrutto :: (Double -> Double -> Double) -> [Neuron] -> Neuron -> Double
activateBrutto f neurons neuron = sum $ zipWith f (neuralScalars neuron) connectionActivities
    where connectionActivities = map netActivation neurons

activateNeuron :: (Double -> Double -> Double) -> (Double -> Double) -> [Neuron] -> Neuron -> Neuron
activateNeuron f sq aneurons neuron = updateNeuron $ sq $ activateBrutto f aneurons neuron
    where updateNeuron = updateNetActivation neuron


-- Network Activation

forwardPropagation :: (Double -> Double -> Double) -> (Double -> Double) -> Network -> Network -> Network
forwardPropagation f sq activated = networkFmap activateNeuron' 
    where activateNeuron' = activateNeuron f sq (networkNeurons activated)

networkPropagation :: (Double -> Double -> Double) -> (Double -> Double) -> Network -> (Network, Network)
networkPropagation _ _ output@OpenNetwork{} = (output, output)
networkPropagation f sq input@(ConnectedNetwork inNeurons network) =
  let propagation = forwardPropagation f sq
      (queriedNetwork, outputLayer) = networkPropagation f sq $ propagation input network
    in (ConnectedNetwork inNeurons queriedNetwork, outputLayer)


-- Querying Network

initializeNetworkQuery :: Network -> [Double] -> Network
initializeNetworkQuery (ConnectedNetwork neurons network) input = ConnectedNetwork activatedNeurons network
    where activatedNeurons = zipWith updateNetActivation neurons input

initializeNetworkQuery (OpenNetwork neurons) input = OpenNetwork activatedNeurons
    where activatedNeurons = zipWith updateNetActivation neurons input

networkQuery :: (Double -> Double -> Double) -> (Double -> Double) -> Network -> [Double] -> (Network, [Double])
networkQuery f sq net inp = (queriedNetwork,  map netActivation . networkNeurons $ responseLayer)
    where initializedNetwork = initializeNetworkQuery net inp
          (queriedNetwork, responseLayer) = networkPropagation f sq initializedNetwork


standardQuery :: Network -> [Double] -> (Network, [Double])
standardQuery = networkQuery (*) squish


-- Learning


neuronGradientDecent :: [Double] -> Double -> Neuron -> (Neuron, Double)
neuronGradientDecent hidden expected neuron = (updateScalars neuron updatedWeigths, derror * dsq)
    where derror = dcostFunction (netActivation neuron) expected
          dsq = dsquish (netActivation neuron)

        --   This is where i apply the chain rule for each Weigth
        --  derror is dTotalCost/dOutput,
        --  dsq is dOutput/dInput
        --  and previous output is dInput/dWeigth
        --  The result equals dTotalCost/dWeigth times a learningRate
          offset = map (\hiddenActivation -> derror * dsq * hiddenActivation * learningRate) hidden

          updatedWeigths = zipWith (-) (neuralScalars neuron) offset 




neuronGradientPropagation :: [Double] -> [Neuron] -> [Double] -> Neuron -> Int -> (Neuron, Double)
neuronGradientPropagation hidden connections dErrordPNoutput neuron index = (updateScalars neuron updatedWeigths, errorDoutput * outputDinput)
    where connections' = map ((!!index) . neuralScalars) connections
        -- Rate of change of total Error with respect to this neurons output
          errorDoutput = sum $ zipWith (*) connections' dErrordPNoutput

        -- Rate of change of This neurons output with respect to its input
          outputDinput = dsquish (netActivation neuron)

          --Same as the NeuronGradientDecent but instead to calculate
          --The error with respect to the weight we use the fact that
          --The Error with Respect to the previous nodes input is already calculated
          --Using that we calculate the error with respect to output for
          --All of the nodes this node is connected to and sum them up.
          --That'll be the error with respect to this nodes output
          --Multiplying that with the rate of change of the output
          --With respect to the input gives dError/dIput
          --Which in turn is used to adjust all the weights

          -- This because dInput/dWeigth == previous Activation
          -- THat's why previous activation * dError/dInput = dError/dWeigth
          -- which gives the direction to minimize the cost
          offset = map (\hiddenActivation -> errorDoutput * outputDinput * hiddenActivation * learningRate) hidden

          updatedWeigths = zipWith (-) (neuralScalars neuron) offset 


backPropagation :: Network -> [Double] -> (Network, [Double])
backPropagation (ConnectedNetwork cneurons (OpenNetwork oneurons)) expected =
  let cactivation = map netActivation cneurons
      (neurons', dErrordOutput) = unzip $ zipWith (neuronGradientDecent cactivation) expected oneurons
    in (ConnectedNetwork cneurons (OpenNetwork neurons'), dErrordOutput)

backPropagation (ConnectedNetwork cneurons network@(ConnectedNetwork cneurons' _)) expected =
  let (network', dErrorsdOutput) = backPropagation network expected
      cactivation = map netActivation cneurons
      neuronGradientPropagation' = neuronGradientPropagation cactivation (networkNeurons network') dErrorsdOutput
      (neurons', dErrorsdOutput') = unzip $ zipWith neuronGradientPropagation' cneurons' [0..]
    in (ConnectedNetwork neurons' network', dErrorsdOutput')


learn :: Network -> [Double] -> Network
learn (ConnectedNetwork cneurons network@(ConnectedNetwork cneurons' _)) expected =
  let (network', dErrorsdOutput) = backPropagation network expected
      cactivation = map netActivation cneurons
      neuronGradientPropagation' = neuronGradientPropagation cactivation (networkNeurons network') dErrorsdOutput
      (neurons', dErrorsdOutput') = unzip $ zipWith neuronGradientPropagation' cneurons' [0..]
    in ConnectedNetwork cneurons (ConnectedNetwork neurons' network')

outputCost :: [Double] -> [Double] -> Double
outputCost output expected = sum $ zipWith costFunction output expected

costFunction :: Double -> Double -> Double
costFunction output expected = (expected - output) * (expected - output)

dcostFunction :: Double -> Double -> Double
dcostFunction output expected =  output - expected

squish :: Double -> Double
squish val = exp val / (exp val + 1)

dsquish :: Double -> Double
dsquish val = squish val * (1 - squish val)

learningRate :: Double
learningRate = 1

multiplePropagations :: Network -> [Double] -> [Double] -> Int -> IO()
multiplePropagations _ _ _ 0 = print "donezo"
multiplePropagations net inp exp iter = do 
                                      let (net', resp) = standardQuery net inp

                                      {-print resp -}
                                      print $ outputCost resp exp

                                      let net'' = learn net' exp

                                      multiplePropagations net'' inp exp (iter - 1)




main :: IO ()
main = do

    let scheme = [3, 5, 5, 3]
    let input = [0.5, 0.7, 0.1]
    let expected = [1.0, 0.0, 0.4]

    seed <- getStdGen 


    putStrLn "Creating Network"
    let network = networkInitializer seed scheme

    multiplePropagations network input expected 20

