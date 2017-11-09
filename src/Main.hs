module Main where

data Connection a = Connection Int a
data Neuron a = Input Int a | Hidden Int a a [Connection a] | Output a a [Connection a]

-- Everything Needed from a Neuron in a NeuralNetwork
class NeuralBehavior a where
    squish ::  a -> a
    dsquish :: (Num b) => a -> b

retrieve :: [a] -> (a -> Bool) -> a
retrieve list f = case dropWhile (not . f) list of
        [] -> error "Non Existant Connection Requested"
        (a:_) -> a

connectionId :: Connection a -> Int
connectionId (Connection identity _) = identity

connectionWeigth :: Connection a -> a
connectionWeigth (Connection _ weigth) = weigth

activateNEU :: Neuron a -> a -> Neuron a
activateNEU (Hidden a _ c d) b = Hidden a b c d
activateNEU (Output _ b c) a = Output a b c
activateNEU (Input a _) b = Input a b

activation :: Neuron a -> a
activation (Hidden _ a _ _) = a
activation (Output a _ _) = a
activation (Input _ a) = a

bias :: Neuron a -> a
bias (Hidden _ _ a _) = a
bias (Output _ a _) = a
bias Input{} = error "Input Neurons has no bias"

neuronId :: Neuron a -> Int
neuronId (Hidden identity _ _ _) = identity
neuronId Output{} = error "Output neurons has no ID because the lack further connections"
neuronId (Input identity _) = identity

connections :: Neuron a -> [Connection a]
connections (Hidden _ _ _ b) = b
connections (Output _ _ b) = b
connections Input{} = error "Input Neurons has no weigths"

connection :: Int -> Neuron a -> Connection a
connection identity (Hidden _ _ _ b) = retrieve b ((==identity) . connectionId)
connection identity (Output _ _ b) = retrieve b ((==identity) . connectionId)
connection _ Input{} = error "Input Neurons has no weigths"

activateNeuron :: (Num a) => Neuron a -> [Neuron a] -> Neuron a
activateNeuron neuron = squish . activateNEU neuron . sum .
    map (\neuron' ->
        let identity = neuronId neuron'
        in (activation neuron' * connectionWeigth (connection identity neuron)))

-------------------------------
-- Everything Needed for Querying and Learning

data Network a = NetworkHidden [Neuron a] (Network a) | NetworkInput [Neuron a] (Network a) | NetworkOutput [Neuron a]

networkNeurons :: Network a -> [Neuron a]
networkNeurons (NetworkHidden neurons _) = neurons
networkNeurons (NetworkInput neurons _) = neurons
networkNeurons (NetworkOutput neurons) = neurons

activateNET :: Network a -> [Neuron a] -> Network a
activateNET (NetworkHidden _ network) neurons = NetworkHidden neurons network
activateNET (NetworkInput _ network) neurons = NetworkInput neurons network
activateNET NetworkOutput{} neurons = NetworkOutput neurons


propagate :: (Num a) => Network a -> Network a
propagate (NetworkInput neurons network) = propagate . activateNET network . map (`activateNeuron` neurons) $ networkNeurons network
propagate (NetworkHidden neurons network) = propagate . activateNET network . map (`activateNeuron` neurons) $ networkNeurons network
propagate network@(NetworkOutput _) = network


--------------------------------------------
-- Everything Needed for instantiating a network

makeConnectedNeurons ::[Int] -> (Int -> a -> a -> b -> Neuron a) ->  a -> a -> Int -> ([Int], [Neuron a])
makeConnectedNeurons identifications constructor init bias cardinality =
    let ids = take cardinality identifications
        neurons = map (\identity -> constructor identity init bias []) ids
    in (drop cardinality identifications, neurons)




instance NeuralBehavior (Neuron a) where
    squish neuron = neuron
    dsquish neuron = 5

cost :: (Num a) => Network a -> [a] -> a
cost (NetworkOutput neurons) = square . sum . zipWith (-) (map activation neurons)
    where square a = a * a




main :: IO ()
main = print "hi"






