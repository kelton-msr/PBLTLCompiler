# This serves as a quick example of how to use this transpiler on real TLA+ specification, and how to interpret the resulting data you get.
First, we look at the specification (contained in `ewd998.tla`) for the variables that we want to use. Our properties use a different logic (PBLTL) which shares some of the same semantics, but has less expresivity in certain ways, and more expressivity in others.
We still use TLA to write the specification itself, so our properties are going to use the regular TLA+ variables and operators to express what we want, even though PBLTL is not TLA+.

Looking at `ewd998.tla,` we see the liveness property
```
Liveness ==
  Termination ~> terminationDetected
 ```
Which is syntax sugar for
```
Liveness ==
  [](Termination => <>terminationDetected)
``` 

Say we want to check this property, but we want to know that termination will be detected within 8 time steps (where 8 is 2x the number of nodes), instead of merely "eventually."
In PBLTL, this would be `[](Termination => <>(<=8)terminationDetected)`. Giving this property to the PBLTL transpiler yields us the following:
```
VARIABLES hasEmittedCSV, c
Property == 
    [](((c["temp"]["counter"] <= 8) \/ c["temp"]["condition"]))
NextP == 
    /\ hasEmittedCSV' = ~(((c["temp"]["counter"] <= 8) \/ c["temp"]["condition"]))
    /\ (IF (hasEmittedCSV' /\ ~(hasEmittedCSV)) THEN CSVWrite("%1$s", <<TRUE>>, "statPropsRes.csv") ELSE TRUE)
    /\ c' = [temp|-> [condition|-> (c["temp"]["condition"] \/ terminationDetected), counter|-> (IF ~(c["temp"]["hasAntecedent"]) THEN 0 ELSE (IF (c["temp"]["condition"] /\ Termination) THEN 1 ELSE (c["temp"]["counter"] + 1))), hasAntecedent|-> (c["temp"]["hasAntecedent"] \/ Termination)]]

InitP == 
    /\ hasEmittedCSV = FALSE
    /\ c = [temp|-> [counter|-> 0, condition|-> terminationDetected, hasAntecedent|-> Termination]]
```
Now that we actually have all of this, how do we use it?

Due to the way that TLC works, we unfortunately cannot just test the property directly, so we will not be using that.
The other two can be used in a pretty straight forward way: putting their definitions at points such that all of the variables they reference are defined, and then `/\`ing them into the standard `Next` and `Init` definitions.
This step may (annoyingly) require a bit of restructuring of the specification: We're accessing variables that are usually used for properties in the specification itself, so we may have to move some of those variables around in the specification.
We do here, moving the definitions for `Termination` and `terminationDetected` up earlier, and varaibles that those definitions use, namely `B` and `Sum`.
N.B: Make sure you **don't** add `c` and `hasEmitted` to any `UNCHANGED` expressions, as they are changed every time `Next` is evaluated.
If you did this, you would end up with `Next` being evaluated to false, which you don't want.

Once we have this, you can then run TLC in simulation mode, where a CSV file called `statPropRes.csv` (short for statistical property results) will be generated, with a `TRUE` every time the property **did not** hold.
TLC itself provides you with the information of how many total runs there have been, which you can then use to calculate the likelihood that the property holds overall.
The certainty that this likelihood is correct and related calculations are currently not implemented.

In this folder, we have the original EWD998 specification (`EWD998.tla`), and the one that has been modified with these new changes (`EWD998_stat.tla`)
Running `EWD998_stat.tla` with TLC in simulation mode allows us to check how often the property specified above holds true, which we can do by executing `tlc -deadlock -simulate EWD998_stat.tla` on the command line.
Now that we have this specification, we can use it differentially to test new changes and optimizations.

`EWD998_opt_stat.tla` contains a specification that implements a proposed optimization.
Staring at it, it perhaps immediately obvious if this is a true optimization, or if it hurts performance.
Thankfully, we can simply see how often the time property is violated after the optimization.

Doing so, we get 1048 violations out of 100,000 runs for the original, for a 1.048% violation rate.
Compare this to the proposed optimization, where we get 12519 violationes out of 100,000 runs, for a 12.519% violation rate. 
That's more than 10 times as many failures! Seems that this is not a good optimization...
In regular TLA+, there's no real good way to verify this (without doing something similar to this by hand), since it fails only sometimes.
This way you can think about the statiscal property you actually want to verify without having to write this TLA+ boilerplate by hand, or learning how to use a statistical model checker.
