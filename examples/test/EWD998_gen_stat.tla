---------- MODULE EWD998_gen_stat ----------
EXTENDS Integers, CSV, EWD998
VARIABLES c, isViolated
OldModuleInstance == INSTANCE EWD998
extendedStatisticalVars == <<c,isViolated,vars>>
NewStatisticalNext ==
    /\ OldModuleInstance!Next
    /\ isViolated' = (~(((c["temp"]["counter"] <= 7) \/ c["temp"]["condition"])) \/ isViolated)
    /\ (IF (isViolated' /\ ~(isViolated)) THEN CSVWrite("%1$s", <<TRUE>>, "statPropsRes.csv") ELSE TRUE)
    /\ c' = [temp|-> [condition|-> (c["temp"]["condition"] \/ terminationDetected), counter|-> (IF ~(c["temp"]["hasAntecedent"]) THEN 0 ELSE (IF (c["temp"]["condition"] /\ Termination) THEN 1 ELSE (c["temp"]["counter"] + 1))), hasAntecedent|-> (c["temp"]["hasAntecedent"] \/ Termination)]]
NewStatisticalInit ==
    /\ OldModuleInstance!Init
    /\ isViolated = FALSE
    /\ c = [temp|-> [counter|-> 0, condition|-> terminationDetected, hasAntecedent|-> Termination]]
NewStatisticalSpec == NewStatisticalInit /\ [][NewStatisticalNext]_extendedStatisticalVars /\ WF_extendedStatisticalVars(NewStatisticalNext)
==========================
