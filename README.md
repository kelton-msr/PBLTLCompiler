# PBLTLC: A compiler from a probabilistic bounded linear temporal logic (PBLTL) to TLA+.


## PBLTL

PBLTL looks like the following:

```S ::= Pr(>=P)(F) | F

F ::= []F | <>(<=N)F | ~F | (F /\ F) | (F => F) | (A variable) | (An operator)
```


Where:

Pr(>=P)(F) represents the proposition that property F holds at least with probability P (for 0 <= P <= 1).
<>(<=N)F represents a "bounded eventually," which expresses the property that F will hold within N time steps.
[] works as expected in temporal logic.


## How to use

Invoke the binary with your property (or a file containing your property), and the name of the TLA+ specification you want to analyze.

If you specify a probability (by having a `Pr(>=P)` at the beginning of your property), then the
binary will emit a new TLA+ specification (to a specified output file, or std. out by default), and a single sampling plan, which is a 2-tuple `(n,c)`.

Note: there is a default set of tolerances (like acceptable likelihood of false positives, etc) which go into calculating this sampling plan.
Check what these are (with `-h`) and change them if you want different guarantees!

WARNING: The algorithm to calculate single sampling plans is quite slow for strong guarantees! 
Coming up with a faster on is on my TODO list, but as it stands it may take a (very very) long time strong guarantees.

You can then run the emitted TLA+ specification with TLC's simulation mode, setting `num=n`.
This will create a file (called `statPropRes.csv` by default), which contains information about simulation results.
You can then either invoke the binary again with the correct flag, or just manually check that number of lines in the statistics file is less than `n-c-1`.

If you don't specify a probability, then just the specification will be emitted. 
It's up to you to decide how many runs should be completed (or if you want to do total model checking).
The simulation results will contain the total number of violations of the property, which you can use how you wish.


### Usage Example
Say you want to verify that a termination detection algorithm (specifically EWD998, as seen in the examples folder) works within 7 time steps with 90% probability.
To verify this, you would first run:

`pbltlc "Pr(>=.9)([](Termination => <>(<=7) terminationDetected))" -i EWD998.tla  -o EWD998_stat.tla`

Which will then emit a new spec to `EWD998_stat.tla`, and output the following to standard out:

(TODO give real values here and make sure it aligns with actual input)
`Single Sampling plan: (n,c)`

This information is also contained within the generated spec.

You can then run the generated spec like so:

`tlc -simulate 'num=n' EWD009_stat.tla`

(adding `-deadlock` and etc. as appropriate)

Wait for that to finish, and then run the following to see if the property you wrote holds:

`pbltlc --tabulate statPropRes.csv`

And you're done!


## Restrictions on PBLTL

Unfortunately, not everything allowed by that grammar above is currently supported by this transpiler.
Currently, the transpiler is only (informally) verified to work for up to 3 nestings of temporal operators. 
It may work for more, but this has not been tested (and I have a feeling that there are cases where it breaks down).
It is also not supported to have bounded diamonds as the antecedent of an implication.
There is also currently not support for unbounded diamonds (like from TLA+).

Fixing each of these is on the roadmap, but are not quite at the top yet.

