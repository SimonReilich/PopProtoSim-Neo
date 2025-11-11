# Proto-Sim

A simulator for population protocols with snipers in Haskell. The program is capable of simulating single executions of different protocols with an optional sniper, that can either randomly snipe agents with a given rate p or be controlled manually be the user. After each execution of a protocol, the program calculates, how many agents less it would take to still arrive at the same output value the execution converged to. If that number is always less or equal to the total number of snipes, the protocol would be strongly robust.

It is also possible to generate statistics where the program tries many different protocols and outputs the number of snipes and the number of agents that would have to be removed at the start to get the same output.