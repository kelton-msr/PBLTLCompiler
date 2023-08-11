-------------------------- MODULE EWD998_mod_stat ---------------------------

EXTENDS Integers, Functions, CSV, EWD998, Sequences

VARIABLES hasEmittedCSV, c
\*Property == 
\*    [](((c["temp"]["counter"] <= 8) \/ c["temp"]["condition"]))

Foo == INSTANCE EWD998

fvars == <<vars,c, hasEmittedCSV>>

cond == c["temp"]["counter"] > 3


FNext == 
    /\ Foo!Next
    /\ c' = [temp|-> [condition|-> (c["temp"]["condition"] \/ Foo!terminationDetected), counter|-> (IF ~(c["temp"]["hasAntecedent"]) THEN 0 ELSE (IF (c["temp"]["condition"] /\ Foo!Termination) THEN 1 ELSE (c["temp"]["counter"] + 1))), hasAntecedent|-> (c["temp"]["hasAntecedent"] \/ Foo!Termination)]]
    /\ hasEmittedCSV' = ~(((c'["temp"]["counter"] <= 8) \/ c'["temp"]["condition"]))
    /\ (IF (hasEmittedCSV' /\ ~(hasEmittedCSV)) THEN CSVWrite("%1$s", <<TRUE>>, "statPropsRes.csv") ELSE TRUE)

FInit ==
    /\ Foo!Init 
    /\ hasEmittedCSV = FALSE
    /\ c = [temp|-> [counter|-> 0, condition|-> Foo!terminationDetected, hasAntecedent|-> Foo!Termination]]

FSpec == FInit /\ [][FNext]_fvars /\ WF_fvars(FNext)

======
