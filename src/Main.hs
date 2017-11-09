module Main where

data Neuron a = OpenNeuron a | ConnectedNeuron a a [a]
data Network a = ConnectedNetwork [Neuron a] (Network a) | OpenNetwork [Neuron a]

-- Accessors
bias :: Neuron a -> a
bias OpenNeuron{} = error "input neuron has no bias"
bias (ConnectedNeuron _ a _) = a

activity :: Neuron a -> a 
activity (OpenNeuron a ) = a
activity (ConnectedNeuron a _ _) = a

scalars :: Neuron a -> [a]
scalars OpenNeuron{} = error "Input neuron has no weigths"
scalars (ConnectedNeuron _ _ a) = a

networkNeurons :: Network a -> [Neuron a]
networkNeurons (OpenNetwork a) = a
networkNeurons (ConnectedNetwork a _) = a

-- GeneratorFunctions

openNeuronGenerator :: a -> [Neuron a]
openNeuronGenerator activation = cycle [(OpenNeuron activation)]

connectedNeuronGenerator :: a -> a -> [([a] -> Neuron a)]
connectedNeuronGenerator activation bias = cycle [(ConnectedNeuron activation bias)]

updateNeuron :: Neuron a -> a -> Neuron a
updateNeuron (OpenNeuron _) a = OpenNeuron a
updateNeuron (ConnectedNeuron _ b c) a = ConnectedNeuron a b c

updateNetwork :: Network a -> [Neuron a] -> Network a
updateNetwork OpenNetwork{} a = OpenNetwork a
updateNetwork (ConnectedNetwork _ b) a = ConnectedNetwork a b

-- NetworkBuilding

networkCardinality :: Network a -> Int
networkCardinality = length . networkNeurons

networkGenerator :: [[a] -> Neuron a] -> a -> Int -> [Int] -> Network a
networkGenerator neuronGenerator defaultWeigth previousCardinality (layer:layers) = case layers of 
    [] -> OpenNetwork neurons
    _  -> let nextNetwork = networkGenerator neuronGenerator defaultWeigth layer layers
          in (ConnectedNetwork neurons nextNetwork)
    
    where neurons = (take layer . map (\f -> f $ replicate previousCardinality defaultWeigth) $ neuronGenerator) 

networkBuilder :: a -> a -> a -> [Int] -> Network a 
networkBuilder defaultActivation defaultBias defaultWeigth (inputLayer:layers) = 
    let openNeurons = take inputLayer $ openNeuronGenerator defaultActivation
        connectedNeurons = connectedNeuronGenerator defaultActivation defaultBias 
    in ConnectedNetwork openNeurons (networkGenerator connectedNeurons defaultWeigth inputLayer layers)

makeDefaultNetwork :: [Int] -> Network Double
makeDefaultNetwork = networkBuilder 0.5 10.0 0.5

--Neuron functionalities

activateConnection :: (a -> a -> a) -> Neuron a -> a -> a
activateConnection f neuron = f (activity neuron) 

activateNeuron :: (Num a) => (Neuron a -> a -> a) -> [Neuron a] -> Neuron a -> Neuron a
activateNeuron f connections neuron = updateNeuron' . sum $ zipWith (f) connections (scalars neuron)
    where 
        updateNeuron' = updateNeuron neuron

--Network functionalities

foldNetwork :: (Network a -> Network a -> Network a) -> Network a -> Network a -> Network a
foldNetwork f a network@(ConnectedNetwork _ network') = foldNetwork f (f a network) network' 

propagateNetwork :: (Num a) => (a -> a -> a) -> Network a -> Network a -> Network a
propagateNetwork f previousNetwork network = updateNetwork' . map (activateNeuron') $ networkNeurons network
    where activateNeuron' = activateNeuron (activateConnection f) (networkNeurons previousNetwork)
          updateNetwork' = updateNetwork network

activateInputNetwork :: Network a -> [a] -> Network a
activateInputNetwork (ConnectedNetwork _ network) input =
    let inputNeurons = map OpenNeuron input 
    in ConnectedNetwork inputNeurons network

activateOpenNetwork :: (Num a) => (a -> a -> a) -> Network a -> [a] -> Network a
activateOpenNetwork f network@(ConnectedNetwork _ networks) input = 
    let activeNetwork = activateInputNetwork network input
        propagation = propagateNetwork f
    in foldNetwork propagation activeNetwork networks











main :: IO ()
main = print "hi"






