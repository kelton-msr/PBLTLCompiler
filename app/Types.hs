module Types where
import Control.Monad.State.Strict
import Data.List (intersperse)
-- I was expecting this to be more or less stateless when writing this but I forgot about names.
-- It also appears to be necessary to keep track of if a wff is under a box,
-- In particular to handle updating the counter.
type CompM a = State ([String],Int) a

-- internal syntactic representation for (PB)LTL formulas
-- PBLTL being this modal logic that has bounded diamonds and one probability check on the outside
-- 
-- Bounded Diamonds only for now
-- Would probably not be a huge deal to add unbounded diamonds, but lets keep it simple for now.
-- Probably need to add some sort of operators syntax here to deal with TLA+ things.
data LTLForm = LBox LTLForm 
             | LDiamond Int LTLForm 
             | LAnd LTLForm LTLForm 
             | LImplies LTLForm LTLForm 
             | LAtom String 
             | LOp String [LTLForm]
             | LNeg LTLForm

instance Show LTLForm where
    show (LBox a)       = "□(" ++ show a ++ ")"
    show (LDiamond n a) = "◇≤" ++ show n ++ "("     ++ show a ++ ")"
    show (LAnd l r)     = show l ++ " ∧ " ++ show r
    show (LImplies l r) = show l ++ " ⇒ " ++ show r
    show (LNeg f)       = "¬(" ++ show f ++ ")"
    show (LOp s fs)     = s ++ "(" ++ (concat $ intersperse "," $ map show fs) ++ ")"
    show (LAtom s)      = s

-- Primes will be represented at the var level for now.
-- internal syntactic representation for (a subset of) TLA+ formulas
data TLAForm = TBox TLAForm 
             | TDiamond TLAForm 
             | TAnd TLAForm TLAForm 
             | TImplies TLAForm TLAForm 
             | TNeg TLAForm 
             | TAssign TLAVal TLAVal 
             | TLeq TLAVal TLAVal 
             | TOr TLAForm TLAForm 
             | TBool Bool 
             | TVal TLAVal 
    deriving Eq

instance Show TLAForm where
    show (TBox a)       = "[](" ++ show a ++ ")"
    show (TDiamond a)   = "<>(" ++ show a ++ ")"
    show (TNeg a)       = "~("  ++ show a ++ ")"
    show (TAnd l r)     = show l ++ " /\\ " ++ show r
    show (TOr l r)      = show l ++ " \\/ " ++ show r
    show (TImplies l r) = show l ++ " => "  ++ show r 
    show (TLeq l r)     = show l ++ " <= "  ++ show r 
    show (TAssign l r)  = show l ++ "' = "  ++ show r
    show (TVal t)       = show t
    show (TBool True)   = "TRUE"
    show (TBool False)  = "FALSE"

-- The distinction between value and formula is more to limit bugs while writing than something principled
-- This could easily be moved into the TLAForm type later
data TLAVal = TInt Int 
            | TFunc [(String,TLAVal)] 
            | TForm TLAForm 
            | TIf TLAForm TLAVal TLAVal 
            | TPlus TLAVal TLAVal
            | TVar String
            | TOp String [TLAVal]
            -- Only one that might not be self explanitory -- this is an application of a TLA+ function to a key.
            -- e.g (TApp (TVar "c") "foo") = c["foo"]
            | TApp TLAVal String 
    deriving Eq

instance Show TLAVal where
    show (TInt i)    = show i 
    show (TForm a)   = show a
    show (TIf f t e) = "(IF " ++ show f ++ " THEN " ++ show t ++ " ELSE " ++ show e ++ ")"
    show (TApp v s)  = show v ++ "[\"" ++ s ++ "\"]"
    show (TPlus v s) = "(" ++ show v ++ " + " ++ show s ++ ")"
    show (TVar s)    = s
    show (TOp s fs)  = s ++ "(" ++ (concat $ intersperse "," $ map show fs) ++ ")"
    show (TFunc vs)   = "[" ++ (concat $ intersperse "," $ map (\(n,v) -> n ++ "|-> " ++ show v) vs)  ++ "]"
