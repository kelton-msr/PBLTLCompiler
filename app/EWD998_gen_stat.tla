---------- MODULE EWD998_gen_stat ----------
EXTENDS Integers, CSV, EWD998
VARIABLES c, isViolated
OldModuleInstance == INSTANCE EWD998
extendedStatisticalVars == <<c,isViolated,vars>>
NewStatisticalNext ==
    /\ OldModuleInstance!Next
    /\ isViolated' = (~(Pr) \/ isViolated)
    /\ (IF (isViolated' /\ ~(isViolated)) THEN CSVWrite("%1$s", <<TRUE>>, "statPropsRes.csv") ELSE TRUE)
    /\ c' = []
NewStatisticalInit ==
    /\ OldModuleInstance!Init
    /\ isViolated = FALSE
    /\ c = []
NewStatisticalSpec == NewStatisticalInit /\ [][NewStatisticalNext]_extendedStatisticalVars /\ WF_extendedStatisticalVars(NewStatisticalNext)
==========================