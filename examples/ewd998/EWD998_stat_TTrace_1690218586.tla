---- MODULE EWD998_stat_TTrace_1690218586 ----
EXTENDS EWD998_stat, Sequences, TLCExt, Toolbox, Naturals, TLC

_expression ==
    LET EWD998_stat_TEExpression == INSTANCE EWD998_stat_TEExpression
    IN EWD998_stat_TEExpression!expression
----

_trace ==
    LET EWD998_stat_TETrace == INSTANCE EWD998_stat_TETrace
    IN EWD998_stat_TETrace!trace
----

_inv ==
    ~(
        TLCGet("level") = Len(_TETrace)
        /\
        c = ([temp |-> [condition |-> FALSE, counter |-> 0, hasAntecedent |-> FALSE]])
        /\
        hasEmittedCSV = (FALSE)
        /\
        color = ((0 :> "white" @@ 1 :> "black" @@ 2 :> "black" @@ 3 :> "black"))
        /\
        pending = ((0 :> 0 @@ 1 :> 0 @@ 2 :> 0 @@ 3 :> 0))
        /\
        active = ((0 :> FALSE @@ 1 :> FALSE @@ 2 :> TRUE @@ 3 :> TRUE))
        /\
        counter = ((0 :> 0 @@ 1 :> 0 @@ 2 :> -1 @@ 3 :> 1))
        /\
        token = ()
    )
----

_init ==
    /\ active = _TETrace[1].active
    /\ color = _TETrace[1].color
    /\ c = _TETrace[1].c
    /\ counter = _TETrace[1].counter
    /\ pending = _TETrace[1].pending
    /\ token = _TETrace[1].token
    /\ hasEmittedCSV = _TETrace[1].hasEmittedCSV
----

_next ==
    /\ \E i,j \in DOMAIN _TETrace:
        /\ \/ /\ j = i + 1
              /\ i = TLCGet("level")
        /\ active  = _TETrace[i].active
        /\ active' = _TETrace[j].active
        /\ color  = _TETrace[i].color
        /\ color' = _TETrace[j].color
        /\ c  = _TETrace[i].c
        /\ c' = _TETrace[j].c
        /\ counter  = _TETrace[i].counter
        /\ counter' = _TETrace[j].counter
        /\ pending  = _TETrace[i].pending
        /\ pending' = _TETrace[j].pending
        /\ token  = _TETrace[i].token
        /\ token' = _TETrace[j].token
        /\ hasEmittedCSV  = _TETrace[i].hasEmittedCSV
        /\ hasEmittedCSV' = _TETrace[j].hasEmittedCSV

\* Uncomment the ASSUME below to write the states of the error trace
\* to the given file in Json format. Note that you can pass any tuple
\* to `JsonSerialize`. For example, a sub-sequence of _TETrace.
    \* ASSUME
    \*     LET J == INSTANCE Json
    \*         IN J!JsonSerialize("EWD998_stat_TTrace_1690218586.json", _TETrace)

=============================================================================

 Note that you can extract this module `EWD998_stat_TEExpression`
  to a dedicated file to reuse `expression` (the module in the 
  dedicated `EWD998_stat_TEExpression.tla` file takes precedence 
  over the module `EWD998_stat_TEExpression` below).

---- MODULE EWD998_stat_TEExpression ----
EXTENDS EWD998_stat, Sequences, TLCExt, Toolbox, Naturals, TLC

expression == 
    [
        \* To hide variables of the `EWD998_stat` spec from the error trace,
        \* remove the variables below.  The trace will be written in the order
        \* of the fields of this record.
        active |-> active
        ,color |-> color
        ,c |-> c
        ,counter |-> counter
        ,pending |-> pending
        ,token |-> token
        ,hasEmittedCSV |-> hasEmittedCSV
        
        \* Put additional constant-, state-, and action-level expressions here:
        \* ,_stateNumber |-> _TEPosition
        \* ,_activeUnchanged |-> active = active'
        
        \* Format the `active` variable as Json value.
        \* ,_activeJson |->
        \*     LET J == INSTANCE Json
        \*     IN J!ToJson(active)
        
        \* Lastly, you may build expressions over arbitrary sets of states by
        \* leveraging the _TETrace operator.  For example, this is how to
        \* count the number of times a spec variable changed up to the current
        \* state in the trace.
        \* ,_activeModCount |->
        \*     LET F[s \in DOMAIN _TETrace] ==
        \*         IF s = 1 THEN 0
        \*         ELSE IF _TETrace[s].active # _TETrace[s-1].active
        \*             THEN 1 + F[s-1] ELSE F[s-1]
        \*     IN F[_TEPosition - 1]
    ]

=============================================================================



Parsing and semantic processing can take forever if the trace below is long.
 In this case, it is advised to uncomment the module below to deserialize the
 trace from a generated binary file.

\*
\*---- MODULE EWD998_stat_TETrace ----
\*EXTENDS EWD998_stat, IOUtils, TLC
\*
\*trace == IODeserialize("EWD998_stat_TTrace_1690218586.bin", TRUE)
\*
\*=============================================================================
\*

---- MODULE EWD998_stat_TETrace ----
EXTENDS EWD998_stat, TLC

trace == 
    <<
    ([c |-> [temp |-> [counter |-> 0, condition |-> FALSE, hasAntecedent |-> FALSE]],hasEmittedCSV |-> FALSE,color |-> (0 :> "white" @@ 1 :> "black" @@ 2 :> "white" @@ 3 :> "black"),pending |-> (0 :> 0 @@ 1 :> 0 @@ 2 :> 0 @@ 3 :> 0),active |-> (0 :> FALSE @@ 1 :> FALSE @@ 2 :> TRUE @@ 3 :> TRUE),counter |-> (0 :> 0 @@ 1 :> 0 @@ 2 :> 0 @@ 3 :> 0),token |-> [pos |-> 0, q |-> 0, color |-> "black"]]),
    ([c |-> [temp |-> [counter |-> 0, condition |-> FALSE, hasAntecedent |-> FALSE]],hasEmittedCSV |-> FALSE,color |-> (0 :> "white" @@ 1 :> "black" @@ 2 :> "white" @@ 3 :> "black"),pending |-> (0 :> 0 @@ 1 :> 0 @@ 2 :> 0 @@ 3 :> 0),active |-> (0 :> FALSE @@ 1 :> FALSE @@ 2 :> FALSE @@ 3 :> TRUE),counter |-> (0 :> 0 @@ 1 :> 0 @@ 2 :> 0 @@ 3 :> 0),token |-> [pos |-> 0, q |-> 0, color |-> "black"]]),
    ([c |-> [temp |-> [counter |-> 0, condition |-> FALSE, hasAntecedent |-> FALSE]],hasEmittedCSV |-> FALSE,color |-> (0 :> "white" @@ 1 :> "black" @@ 2 :> "white" @@ 3 :> "black"),pending |-> (0 :> 0 @@ 1 :> 0 @@ 2 :> 1 @@ 3 :> 0),active |-> (0 :> FALSE @@ 1 :> FALSE @@ 2 :> FALSE @@ 3 :> TRUE),counter |-> (0 :> 0 @@ 1 :> 0 @@ 2 :> 0 @@ 3 :> 1),token |-> [pos |-> 0, q |-> 0, color |-> "black"]]),
    ([c |-> [temp |-> [condition |-> FALSE, counter |-> 0, hasAntecedent |-> FALSE]],hasEmittedCSV |-> FALSE,color |-> (0 :> "white" @@ 1 :> "black" @@ 2 :> "black" @@ 3 :> "black"),pending |-> (0 :> 0 @@ 1 :> 0 @@ 2 :> 0 @@ 3 :> 0),active |-> (0 :> FALSE @@ 1 :> FALSE @@ 2 :> TRUE @@ 3 :> TRUE),counter |-> (0 :> 0 @@ 1 :> 0 @@ 2 :> -1 @@ 3 :> 1),token |-> ])
    >>
----


=============================================================================

---- CONFIG EWD998_stat_TTrace_1690218586 ----
CONSTANTS
    N = 4

INVARIANT
    _inv

CHECK_DEADLOCK
    \* CHECK_DEADLOCK off because of PROPERTY or INVARIANT above.
    FALSE

INIT
    _init

NEXT
    _next

CONSTANT
    _TETrace <- _trace

ALIAS
    _expression
=============================================================================
\* Generated on Mon Jul 24 10:09:47 PDT 2023