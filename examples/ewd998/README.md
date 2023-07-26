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

Say we want to check this property, but we want to know that termination will be detected within 4 time steps, instead of merely "eventually."
In PBLTL, this would be `[](Termination => <>(<=4)terminationDetected)`. Giving this property to the PBLTL transpiler yields us the following:
```
VARIABLES hasEmittedCSV, c
Property == 
    [](((c["temp"]["counter"] <= 4) \/ c["temp"]["condition"]))
NextP == 
    /\ hasEmittedCSV' = ~(((c["temp"]["counter"] <= 4) \/ c["temp"]["condition"]))
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
`hasEmittedCSV` and `c` also need to be added to the various `UNCHANGED` clauses where they apply, and any lists like `vars`.
N.B: Doing this in one place in particular (line 128) gave me what appeared to be a lexing error, while taking out each element from the list and explicitly stating the lack of change worked.
Not sure what's going on there.

Once we have this, you can then run TLC in simulation mode, where a CSV file called `statPropRes.csv` (short for statistical property results) will be generated, with a `TRUE` every time the property **did not** hold.
TLC itself provides you with the information of how many total runs there have been, which you can then use to calculate the likelihood that the property holds overall.
The certainty that this likelihood is correct and related calculations are currently not implemented.

In this folder, we have the original EWD998 specification (`EWD998.tla`), and the one that has been modified with these new changes (`EWD998_stat.tla`)

Now that we have this specification, we can use it differentially to test new changes and optimizations.
