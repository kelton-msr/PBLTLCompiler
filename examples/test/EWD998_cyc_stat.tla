-------------------------- MODULE EWD998_cyc_stat ---------------------------

EXTENDS Integers, Functions, CSV, EWD998, Sequences

VARIABLES hasEmittedCSV, c
\*Property == 
\*    [](((c["temp"]["counter"] <= 8) \/ c["temp"]["condition"]))


fvars == <<vars,c, hasEmittedCSV>>

cond == c["temp"]["counter"] > 3

FNext == 
    /\ Next
    /\ c' = [temp|-> [condition|-> (c["temp"]["condition"] \/ terminationDetected), counter|-> (IF ~(c["temp"]["hasAntecedent"]) THEN 0 ELSE (IF (c["temp"]["condition"] /\ Termination) THEN 1 ELSE (c["temp"]["counter"] + 1))), hasAntecedent|-> (c["temp"]["hasAntecedent"] \/ Termination)]]
    /\ hasEmittedCSV' = ~(((c["temp"]["counter"] <= 8) \/ c["temp"]["condition"]))
    /\ (IF (hasEmittedCSV' /\ ~(hasEmittedCSV)) THEN CSVWrite("%1$s", <<TRUE>>, "statPropsRes.csv") ELSE TRUE)

FInit ==
    /\ Init 
    /\ hasEmittedCSV = FALSE
    /\ c = [temp|-> [counter|-> 0, condition|-> terminationDetected, hasAntecedent|-> Termination]]

\*FSpec == Init /\ [][FNext]_fvars /\ WF_fvars(FNext)

======
