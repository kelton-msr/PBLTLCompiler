module Compiler where
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import Types
import Debug.Trace (trace)
import qualified Data.Map.Strict as Map


-- things that ought be arguments soon
csvFile :: String
csvFile = "statPropsRes.csv"

c :: String
c = "c"
--
-- This deals with the full set of PBLTL formulas, with three exceptions
-- 1. Currently there is no way of having implications where there is a bounded diamond on the left hand side.
-- There some technical details that make the approach used here not work all the way, in particular involving the gnarly case where
-- the bound on the lhs is greater than the bound on the rhs. 
-- 2. There is no way to translate embedded implications with temporal operators. E.g [](P => [](Q => <>(<=n) R))
-- 3. There is no way to express the probability check on the outside within a TLA+ formula.
-- That needs to be handled seperately.



-- Counts the number of (bounded) diamonds in a formula.
-- Useful for determining if we need to compile a formula or can let TLA+ handle it.
diamonds :: LTLForm -> Int
diamonds (LBox f) = diamonds f
diamonds (LBDiamond _ s) = 1 + diamonds s
diamonds (LAnd f g) = diamonds f + diamonds g
diamonds (LImplies f g) = diamonds f + diamonds g
diamonds (LNeg f) = diamonds f
diamonds (LOp _ f) = sum $ diamonds <$> f
diamonds (LAtom _) = 0
diamonds (LInt _) = 0
diamonds (LBinOp l _ r) = diamonds l + diamonds r
diamonds (LDiamond f) = diamonds f

-- This repitition is bad and hacky. TODO.
boxes :: LTLForm -> Int
boxes (LBox f) = 1 + boxes f
boxes (LBDiamond _ s) =  boxes s
boxes (LDiamond s) =  boxes s
boxes (LAnd f g) = boxes f + boxes g
boxes (LImplies f g) = boxes f + boxes g
boxes (LNeg f) = boxes f
boxes (LOp _ f) = sum $ boxes <$> f
boxes (LAtom _) = 0
boxes (LInt _) = 0
boxes (LBinOp l _ r) = boxes l + boxes r

-- Performs simplifications (that should all be tautological) of (PB)LTL formulas.
-- Reduces the number of cases we need to account for in compilation.
simplify :: LTLForm -> LTLForm
simplify (LBox (LBDiamond n f)) = LBDiamond n $ LBox $ simplify f
simplify (LBox (LBox  f)) =  simplify $ LBox f
simplify (LBDiamond n (LBDiamond m f)) = LBDiamond (n+m) $ simplify f
simplify (f `LImplies` (g `LAnd` h)) = simplify ((f `LImplies` g)) `LAnd` (simplify (f `LImplies` h))
simplify (f `LImplies` (g `LImplies` h)) = simplify $ ((f `LAnd` g)) `LImplies` h
simplify (LBox (f `LAnd` g)) = (LBox $ simplify f) `LAnd` (LBox $ simplify g)
-- Not sure about this one... the has diamond constraint is on the right trail but might be overly conservative or overly liberal
simplify (LBox ((LBox f) `LImplies` g)) = if diamonds f == 0 then  (simplify $ LBox f) `LImplies` (simplify $ LBox g) else (LBox $ (simplify $ LBox f) `LImplies` simplify g)
simplify (f `LAnd` g) = (simplify f) `LAnd` (simplify g)
simplify (f `LImplies` g) = (simplify f) `LImplies` (simplify g)
simplify (LNeg f) = LNeg $ simplify f
simplify f = f


-- assumes pre-simplified formula
compileProperty' :: LTLForm -> CompM TLAForm
compileProperty' (LNeg f) = TNeg <$> compileProperty' f
compileProperty' (LInt i) = pure $ TVal $ TInt i
compileProperty' (LAtom s) = pure $ TVal $ TVar s
-- This Formula vs Val stuff is becoming more trouble than it's worth... a refactor would be wise, but for now it works.
compileProperty' (LOp s fs) =  TVal <$> TOp s <$> (mapM ((TForm <$>) . compileProperty')  fs)
compileProperty' (LBinOp l o r ) = do
    l' <- compileProperty' l
    r' <- compileProperty' r
    pure $ (TBinOp (TForm $ l') o (TForm $ r'))
compileProperty' (LImplies l (LBDiamond n r)) = do
        m <- getAndUpdate
        pure $ TBox $ (counter m `tleq` TInt n) `TOr` (TVal $ condition m)
compileProperty' (LImplies l r) = do
        l' <- compileProperty' l
        r' <- compileProperty' r
        pure $ l' `TImplies` r'
compileProperty' (LAnd l r) = do
    l' <- compileProperty' l
    r' <- compileProperty' r
    pure $ l' `TAnd` r'
-- don't need what's inside here, that gets used when determining how `condition m` gets updated in compileNext'
compileProperty' (LBDiamond n _) = do
    m <- getAndUpdate 
    pure $ TBox $ (counter m `tleq` TInt n) `TOr` (TVal $ condition m)
compileProperty' (LDiamond f) = TDiamond <$> compileProperty' f
compileProperty' (LBox s) = do
    if diamonds s > 0
        then 
            --just ignore it? TODO this seems off
            compileProperty' s
        else do
            --TODO this is straight up wrong
            n <- getAndUpdate
            r <- compileProperty' s
            pure $ TVal $ condition n

-- This feels too simple... probably something I'm missing...
compileInit' :: LTLForm -> CompM [TLAForm]
compileInit' (LBinOp _ _ _) = pure []
compileInit' (LInt _)  = pure []
compileInit' (LAtom _) = pure []
compileInit' (LOp _ _) = pure []
compileInit' (LNeg f) = compileInit' f
compileInit' (LAnd l r) = do
    l' <- compileInit' l
    r' <- compileInit' r
    pure $ l' ++ r'
compileInit' (LBox f@(LImplies l r)) =
   if diamonds l + diamonds r == 0 
      then do
            name <- getAndUpdate
            d <- get
            r <- compileInit' f
            pure $ (condition name `TEq` (TForm $ compilePartialProperty f d)) : r
      else do
           name <- getCurrentName
           -- This will generate the incorrect names if `l` has a diamond for now. 
           l' <- compileInit' l 
           r' <- compileInit' r
           d <- get
           pure $ (hasAntecedent name `TEq` (TForm $ compilePartialProperty l d)) : l' ++ r'
compileInit' (LImplies l r) = do
   l' <- compileInit' l 
   r' <- compileInit' r
   pure $ l' ++ r'
    -- if diamonds r > 0
    --    -- TODO deal with left diamonds, too complex for now.
    --    then compileInit' r
       -- else do 
-- not thought in too much detail for composit formulas
compileInit' (LBDiamond _ (LBox f)) = do
   name <- getAndUpdate
   d <- get
   r <- compileInit' f
   --TODO should this compilePartial advance the state?
   pure $ [condition name `TEq` (TForm $ compilePartialProperty f d), counter name `TEq` TInt 0] ++ r
compileInit' (LBDiamond _ f) = do
   name <- getAndUpdate
   d <- get
   r <- compileInit' f
   --TODO should this compilePartial advance the state?
   pure $ [condition name `TEq` (TForm $ compilePartialProperty f d), counter name `TEq` TInt 0] ++ r
compileInit' (LBox f) = do
    name <- getAndUpdate
    d <- get
    r <- compileInit' f
    pure $ (condition name `TEq` (TForm $ compilePartialProperty f d)) : r
compileInit' (LDiamond f) = 
    pure []

-- I think this is where most of the complexity is...
compileNext' :: LTLForm -> CompM [TLAForm]
compileNext' (LAtom _) = pure []
compileNext' (LOp _ _) = pure []
compileNext' (LInt _)  = pure []
compileNext' (LBinOp _ _ _) = pure []
-- Only case where this wouldn't work would be (Q => (<>(<=m)P/\<>(<=n)R)) or so, but this should have been transformed away in simplify.
compileNext' (l `LAnd` r) = do 
    l' <- compileNext' l 
    r' <- compileNext' r
    pure $ l' ++ r'
compileNext' (LBDiamond _ (LBox f)) = do
    d <- get
    name <- getAndUpdate
    let t = compilePartialProperty f d
    f' <- compileNext' f
    pure $ (condition name `TEq` TForm t ) 
            : (counter name `TEq` (TIf (TNeg $ TVal $ condition name) (TInt 0) (counter name `TPlus` TInt 1)))
            : f'
compileNext' (LBDiamond _ f) = do
    d <- get
    --TODO do we update here?
    name <- getAndUpdate
    let t = compilePartialProperty f d
    f' <- compileNext' f
    pure $ (condition name `TEq` TForm (t `TOr` (TVal $ condition name))) 
            : (counter name `TEq` (TIf (TNeg $ TVal $ condition name) (TInt 0) (counter name `TPlus` TInt 1)))
            : f'
compileNext' (l `LImplies` r) = do
    l' <- compileNext' l
    r' <- compileNext' r
    pure $ l' ++ r'
-- This feels inelegant but doing it more general ended up worse, so here we are
compileNext' (LBox imp@(l `LImplies` r)) = do
    if diamonds l > 0 then error "Bounded diamonds in left of implication not supported yet" else pure ()
    -- TODO messy. clean up.
    (names,i) <- get
    name <- getAndUpdate
    let q = compilePartialProperty l (names,i)
    l' <- compileNext' l
    if diamonds r == 0 && boxes r == 0
       --need to treat like a regular box
        then do
            r' <- compileNext' r
            pure $  l' ++ r' ++ [condition name `TEq` TForm ((TVal $ condition name) `TAnd` compilePartialProperty imp (names, i))]
        else do
            dd <- get
            let p = case r of 
                      LBDiamond n (LBox f) -> compilePartialProperty f dd
                      LBDiamond n f -> compilePartialProperty f dd
                      _ -> error $ "Diamonds must be at highest level in consequent. Simplification should have made this the case for " ++ show r
            -- let p = compilePartialProperty r dd
            let nexthasAntecedent = (hasAntecedent name `TEq` TForm ((TVal $ hasAntecedent name) `TOr` q)) 
            let nextCounter  = counter name `TEq` TIf (TNeg (TVal $ hasAntecedent name)) 
                        (TInt 0) 
                        -- TODO Probably should be a direct match instead of `boxes`. Would be a quick refactor, but let's get this done first...
                        (if boxes r > 0 then (counter name) `TPlus` TInt 1 
                                        else (TIf ((TVal $ condition name) `TAnd` q ) (TInt 1) (counter name `TPlus` TInt 1)))
            let nextT = condition name `TEq` if boxes r > 0 then TForm p else TForm ((TVal $ condition name) `TOr` p)
            -- I think we do need to incorperate this for embedded implication.
            -- r' <- compileNext' r
            pure $ nexthasAntecedent:nextCounter:[nextT]
compileNext' (LNeg f) = compileNext' f
-- Works because we assume that there is no nesting of bounded diamonds and unbounded diamonds
compileNext' (LDiamond f) = pure []
compileNext' (LBox f) =
    if diamonds f == 0 
       then do
           name <- getAndUpdate
           (names,index) <- get
           pure $ [condition name `TEq` TForm ((TVal $ condition name) `TAnd` compilePartialProperty f (names, index))]
       else compileNext' f


compilePartialProperty :: LTLForm -> ([String],Int) -> TLAForm
compilePartialProperty f = fst . runState (compileProperty' f) 
compileProperty :: LTLForm -> TLAForm
compileProperty f = compilePartialProperty (simplify f) (names (simplify f),0)

compileNext :: LTLForm -> [TLAForm]
compileNext f = fst $ runState (compileNext' (simplify f)) (names (simplify f),0)

-- bug with boxes TODO
-- hasEmitted nees to be specified in next, and init
compileInit :: LTLForm -> [TLAForm]
compileInit f = (TVar "isViolated" `TEq` TBool False) : (fst $ runState (compileInit' (simplify f)) (names (simplify f),0))


-- should work for most properties. Won't work if there is more than just the one box on the outside. 
-- normally would not need to be made into an invariant, but needs to to deal with a TLC bug
propertyToCSVNext :: TLAForm -> [TLAForm]
propertyToCSVNext (TBox f) = propertyToCSVNext f
-- old impl. Doesn't quite work with how TLA+ does.
-- propertyToCSVNext f = 
--     ((TNeg f) `TAnd` (TNeg $ TVal $ TVar "isViolated")) `TImplies` 
--         ((TVal $ TOp "CSVWrite" [TString "%1$s",TSeq [TBool True],TString csvFile]) `TAnd` (TVar "isViolated'" `TEq` (TBool True)))
propertyToCSVNext f = 
         (TVar "isViolated'" `TEq` TForm ((TNeg f) `TOr` (TVal $ TVar "isViolated"))) : 
            TVal (TIf ((TVal $ TVar "isViolated'") `TAnd` (TNeg $ TVal $ TVar "isViolated")) writeCSV (TBool True)) : []
writeCSV :: TLAVal
writeCSV = 
          TOp "CSVWrite" [TString "%1$s",TSeq [TBool True],TString csvFile]

-- How i've been thinking about setting/updating counters during Next and Init was very much of the form c["blah"]["counter"]' = ...
-- Which is useful for making compilation more modular and easier to reason about, but is not good TLA+.
-- Here, we take all of these "bogus" assignments and turn them into one big assignment that is perhaps long and ugly, but should work.
collateAssignments :: String -> [TLAForm] -> [TLAForm]
collateAssignments var fs = collateAssignments' var fs Map.empty

collateAssignments' :: String -> [TLAForm] -> Map String [(String,TLAVal)] -> [TLAForm]
collateAssignments' var (((TApp (TApp (TVar "c") s) l) `TEq` r):fs) m = collateAssignments' var fs (Map.insertWith (++) s [(l,r)] m) 
collateAssignments' var (h:t) m = h : collateAssignments' var t m
collateAssignments' var [] m = [TVar var `TEq` (TFunc $ map (\(n,v) -> (n, TFunc v)) $ Map.assocs m)]

-- helpers..
counter :: String -> TLAVal 
counter s = (TApp (TApp (TVar "c") s) "counter")
condition :: String -> TLAVal
condition s = TApp (TApp (TVar "c") s) "condition"
hasAntecedent :: String -> TLAVal
hasAntecedent s = TApp (TApp (TVar "c") s) "hasAntecedent"

-- State Helpers
--Updates gets the current name for the counter and updates to the next one
getAndUpdate :: CompM String
getAndUpdate = get >>= \(ss,i) -> put (ss,i+1) >> pure (ss !! i)

getCurrentName :: CompM String
getCurrentName = get >>= \(ss,i) -> pure (ss !! i)

-- dumb name generator for counter.
-- Leads to terrible names, but is dead simple. Good enough for a prototype/MVP.
names :: LTLForm -> [String]
names f = take (diamonds f + boxes f) $ iterate (++"P") "temp"

