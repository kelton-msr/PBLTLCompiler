A compiler from a probabalistic bounded linear temporal logic (PBLTL) to TLA+.

PBLTL looks like the following:

S ::= Pr(>=P)(F) | F

F ::= []F | <>(<=N)F | ~F | (F /\ F) | (F => F) | (A variable) | (An operator)


Where:

Pr(>=P)(F) represents the proposition that property F holds at least with probability P (for 0 <= P <= 1).
<>(<=N)F represents a "bounded eventually," which expresess the property that F will hold within N time steps.
[] works as expected in temporal logic.

The current implementation has a couple restrictions on this general schema which is documented in the source code.

It might be easy to extend this logic with an unbounded <>, but this has not been explored yet.
