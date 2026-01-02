# Proto-Sim

A simulator for population protocols with snipers in Haskell. The program is capable of simulating single executions of different protocols with an optional sniper, that can either randomly snipe agents with a given rate p or be controlled manually be the user. After each execution of a protocol, the program calculates, how many agents less it would take to still arrive at the same output value the execution converged to. If that number is always less or equal to the total number of snipes, the protocol would be strongly robust.

It is also possible to generate statistics where the program tries many different protocols and outputs the number of snipes and the number of agents that would have to be removed at the start to get the same output.

This tool was part of the authors research paper for the TUMKolleg-programm.

# Abstract

Population protocols are a model of distributed computation with very strict limitations on the memory of the single agents. A population protocol is called robust if it is resilient against interference to a certain degree. We expand on the term of robustness, adapting it to protocols with non-binary outputs that compute functions instead of predicates, tweak the existing protocols from https://arxiv.org/abs/2412.11783 to fit our new definition of robustness, and arrive at a general construction for robust protocols for the parallel composition of functions. By that, we partly address whether there exist robust population protocols for Boolean combinations of Presburger predicates since a population protocol computing such predicates is a direct consequence of this. A simulator for population protocols with snipers was implemented, and its results were analyzed to empirically verify the theoretical claims.


# Usage:
```
proto-sim cut [-smdrn] <x0> <t>

proto-sim mod [-smdrn] <x0> <m>

proto-sim cmb [-smdrn] <x0> <m> <t>

proto-sim p [-smdrn] (<xi> <mi> <ti>)...

proto-sim cutstat <x0Max> <tMin> <tMax> -srpn

proto-sim modstat <x0Max> <mMin> <mMax> -srpn

proto-sim cmbstat <x0Max> <mMin> <mMax> <tMin> <tMax> -srpn

proto-sim pstat <x0Max> <mMin> <mMax> <tMin> <tMax> -srpn

proto-sim -h
```

# Options:

- `-s=<rt>, --sniper=<rt>`    Enables the sniper with rate `<rt>`

- `-m, --manual`              Enables the manual sniper, not compatible with `-d`.

- `-d=<dl>, --delay=<dl>`     Prints the execution steps with delay `<dl>` [default: `0.0`].

- `-r=<sd>, --random=<sd>`    Custom seed for random number generation.

- `-p=<fl>, --path=<fl>`      Path of the output file [default: `out.dat`].

- `-n, --nocheck`             Disable the check for convergence, necessary for larger inputs.

- `-h, --help`                Show this screen.