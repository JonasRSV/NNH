# Haskell

Download stack and run "stack runghc src/Main.hs" to see this slow neuralnetwork attempt
to learn to recognize the number three represented in a 3x5 matrix. 

This is not supposed to be used for anything actually useful.. cause in it's current state
this network's learning is quite slow. This was mainly made to educate myself about neural-networks.

The schematic creates the network and each position in the list specifies the amount of Neurons at that given layer.

## Getting Creative..

The network is implemented such that changing squishification functions and Monoid for the Connections and neurons 
is easily changleable.. per default it uses the sigmoid for squishification and multiplication as Monoid. But..
who knows.. perhaps addition is more efficient for learning.. or something else?

# 
