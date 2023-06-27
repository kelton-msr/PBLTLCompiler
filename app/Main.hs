module Main where
import System.Environment (getArgs)
import Text.Megaparsec (parse)
import Types
import Parser
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
-- calulcating what variables must be introduced (just c ?), stringing together with main function.
-- slightly longer term: Take in original spec to check for name collisions/mend the spec directly.
-- For now we will just emit with the currently chosen names and let the programmer add the ammendments.
-- Output will be TLA+ definitions for a new formula.

-- This deals with the full set of PBLTL formulas, with three exceptions
-- 1. Currently there is no way of having implications where there is a bounded diamond on the left hand side.
-- There some technical details that make the approach used here not work all the way, in particular involving the gnarly case where
-- the bound on the lhs is greater than the bound on the rhs. 
-- 2. There is no way to translate embedded implications with temporal operators. E.g [](P => [](Q => <>(<=n) R))
-- 3. There is no way to express the probability check on the outside within a TLA+ formula.
-- That needs to be handled seperately.

-- For a bit more details on PBLTL, see the readme.

-- Counts the number of (bounded) diamonds in a formula.
-- Useful for determining if we need to compile a formula or can let TLA+ handle it.
diamonds :: LTLForm -> Int
diamonds (LBox f) = diamonds f
diamonds (LDiamond _ s) = 1 + diamonds s
diamonds (LAnd f g) = diamonds f + diamonds g
diamonds (LImplies f g) = diamonds f + diamonds g
diamonds (LNeg f) = diamonds f
diamonds (LOp _ f) = sum $ diamonds <$> f
diamonds (LAtom _) = 0

-- This repitition is bad and hacky. TODO.
boxes :: LTLForm -> Int
boxes (LBox f) = 1 + boxes f
boxes (LDiamond _ s) =  boxes s
boxes (LAnd f g) = boxes f + boxes g
boxes (LImplies f g) = boxes f + boxes g
boxes (LNeg f) = boxes f
boxes (LOp _ f) = sum $ boxes <$> f
boxes (LAtom _) = 0

-- Performs simplifications (that should all be tautological) of (PB)LTL formulas.
-- Reduces the number of cases we need to account for in compilation.
simplify :: LTLForm -> LTLForm
simplify (LBox (LDiamond n f)) = LDiamond n $ LBox $ simplify f
simplify (LBox (LBox  f)) =  simplify $ LBox f
simplify (LDiamond n (LDiamond m f)) = LDiamond (n+m) $ simplify f
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
compileCondition' :: LTLForm -> CompM TLAForm
compileCondition' (LNeg f) = TNeg <$> compileCondition' f
compileCondition' (LAtom s) = pure $ TVal $ TVar s
-- This Formula vs Val stuff is becoming more trouble than it's worth... a refactor would be wise, but for now it works.
compileCondition' (LOp s fs) =  TVal <$> TOp s <$> (mapM ((TForm <$>) . compileCondition')  fs)
compileCondition' (LImplies l (LDiamond n r)) = do
        m <- getAndUpdate
        pure $ TBox $ (counter m `TLeq` TInt n) `TOr` (TVal $ truth m)
compileCondition' (LImplies l r) = do
        l' <- compileCondition' l
        r' <- compileCondition' r
        pure $ l' `TImplies` r'
compileCondition' (LAnd l r) = do
    l' <- compileCondition' l
    r' <- compileCondition' r
    pure $ l' `TAnd` r'
-- don't need what's inside here, that gets used when determining how `truth m` gets updated in compileNext'
compileCondition' (LDiamond n _) = do
    m <- getAndUpdate 
    pure $ TBox $ (counter m `TLeq` TInt n) `TOr` (TVal $ truth m)
compileCondition' (LBox s) = do
    if diamonds s > 0
        then 
            compileCondition' s
        else do
            r <- compileCondition' s
            pure $ TBox r

-- This feels too simple... probably something I'm missing...
compileInit' :: LTLForm -> CompM [TLAForm]
compileInit' (LAtom _) = pure []
compileInit' (LOp _ _) = pure []
compileInit' (LNeg f) = compileInit' f
compileInit' (LAnd l r) = do
    l' <- compileInit' l
    r' <- compileInit' r
    pure $ l' ++ r'
compileInit' (LBox (LImplies l r)) = do
   name <- getCurrentName
   -- This will generate the incorrect names if `l` has a diamond for now. 
   l' <- compileInit' l 
   r' <- compileInit' r
   d <- get
   pure $ (hasPredicate name `TAssign` (TForm $ compilePartialCondition l d)) : l' ++ r'
compileInit' (LImplies l r) = do
   l' <- compileInit' l 
   r' <- compileInit' r
   pure $ l' ++ r'
    -- if diamonds r > 0
    --    -- TODO deal with left diamonds, too complex for now.
    --    then compileInit' r
       -- else do 
-- not thought in too much detail for composit formulas
compileInit' (LDiamond _ f) = do
   name <- getAndUpdate
   d <- get
   r <- compileInit' f
   --TODO should this compilePartial advance the state?
   pure $ [truth name `TAssign` (TForm $ compilePartialCondition f d), counter name `TAssign` TInt 0] ++ r
compileInit' (LBox f) = compileInit' f

-- I think this is where most of the complexity is...
compileNext' :: LTLForm -> CompM [TLAForm]
compileNext' (LAtom _) = pure []
compileNext' (LOp _ _) = pure []
-- Only case where this wouldn't work would be (Q => (<>(<=m)P/\<>(<=n)R)) or so, but this should have been transformed away in simplify.
compileNext' (l `LAnd` r) = do 
    l' <- compileNext' l 
    r' <- compileNext' r
    pure $ l' ++ r'
compileNext' (LDiamond _ (LBox f)) = do
    d <- get
    name <- getAndUpdate
    let t = compilePartialCondition f d
    f' <- compileNext' f
    pure $ (truth name `TAssign` TForm t ) 
            : (counter name `TAssign` (TIf (TNeg $ TVal $ truth name) (TInt 0) (counter name `TPlus` TInt 1)))
            : f'
compileNext' (LDiamond _ f) = do
    d <- get
    --TODO do we update here?
    name <- getAndUpdate
    let t = compilePartialCondition f d
    f' <- compileNext' f
    pure $ (truth name `TAssign` TForm (t `TOr` (TVal $ truth name))) 
            : (counter name `TAssign` (TIf (TNeg $ TVal $ truth name) (TInt 0) (counter name `TPlus` TInt 1)))
            : f'
compileNext'  (l `LImplies` r) = do
    l' <- compileNext' l
    r' <- compileNext' r
    pure $ l' ++ r'
-- This feels inelegant but doing it more general ended up worse, so here we are
compileNext' (LBox (l `LImplies` r)) = do
    if diamonds l > 0 then error "Diamonds in left of implication not supported yet" else pure ()
    -- TODO messy. clean up.
    (names,i) <- get
    let name = names !! i
    let q = compilePartialCondition l (names,i)
    l' <- compileNext' l
    if diamonds r == 0
        then do
            r' <- compileNext' r
            pure $ l' ++ r'
        else do
            dd <- get
            let p = case r of 
                      LDiamond n (LBox f) -> compilePartialCondition f dd
                      LDiamond n f -> compilePartialCondition f dd
                      _ -> error $ "Diamonds must be at highest level in consequent. Simplification should have made this the case for " ++ show r
            -- let p = compilePartialCondition r dd
            let nexthasPredicate = (hasPredicate name `TAssign` TForm ((TVal $ hasPredicate name) `TOr` q)) 
            let nextCounter  = counter name `TAssign` TIf (TNeg (TVal $ hasPredicate name)) 
                        (TInt 0) 
                        -- TODO Probably should be a direct match instead of `boxes`. Would be a quick refactor, but let's get this done first...
                        (if boxes r > 0 then (counter name) `TPlus` TInt 1 
                                        else (TIf ((TVal $ truth name) `TAnd` q ) (TInt 1) (counter name `TPlus` TInt 1)))
            let nextT = truth name `TAssign` if boxes r > 0 then TForm p else TForm ((TVal $ truth name) `TOr` p)
            -- I think we do need to incorperate this for embedded implication.
            -- r' <- compileNext' r
            pure $ nexthasPredicate:nextCounter:[nextT]
compileNext' (LNeg f) = compileNext' f
compileNext' (LBox f) =
    if diamonds f == 0 
       then pure []
       else compileNext' f


compilePartialCondition :: LTLForm -> ([String],Int) -> TLAForm
compilePartialCondition f = fst . runState (compileCondition' f) 
compileCondition :: LTLForm -> TLAForm
compileCondition f = compilePartialCondition (simplify f) (names (simplify f),0)

compileNext :: LTLForm -> [TLAForm]
compileNext f = fst $ runState (compileNext' (simplify f)) (names (simplify f),0)

compileInit :: LTLForm -> [TLAForm]
compileInit f = fst $ runState (compileInit' (simplify f)) (names (simplify f),0)

-- How i've been thinking about setting/updating counters during Next and Init was very much of the form c["blah"]["counter"]' = ...
-- Which is useful for making compilation more modular and easier to reason about, but is not good TLA+.
-- Here, we take all of these "bogus" assignments and turn them into one big assignment that is perhaps long and ugly, but should work.

collateAssignments :: [TLAForm] -> TLAForm
collateAssignments fs = collateAssignments' fs Map.empty

collateAssignments' :: [TLAForm] -> Map String [(String,TLAVal)] -> TLAForm
collateAssignments' (((TApp (TApp (TVar "c") s) l) `TAssign` r):fs) m = collateAssignments' fs (Map.insertWith (++) s [(l,r)] m) 
collateAssignments' (x:_) _ = error $ "Something has gone wrong. Saw " ++ show x ++ " in collateAssignents"
collateAssignments' [] m = TVar "c" `TAssign` (TFunc $ map (\(n,v) -> (n, TFunc v)) $ Map.assocs m)

-- helpers..
counter :: String -> TLAVal 
counter s = (TApp (TApp (TVar "c") s) "counter")
truth :: String -> TLAVal
truth s = TApp (TApp (TVar "c") s) "t"
hasPredicate :: String -> TLAVal
hasPredicate s = TApp (TApp (TVar "c") s) "hasPredicate"

-- State Helpers
--Updates gets the current name for the counter and updates to the next one
getAndUpdate :: CompM String
getAndUpdate = get >>= \(ss,i) -> put (ss,i+1) >> pure (ss !! i)

getCurrentName :: CompM String
getCurrentName = get >>= \(ss,i) -> pure (ss !! i)

-- dumb name generator for counter.
-- Leads to terrible names, but is dead simple. Good enough for a prototype/MVP.
names :: LTLForm -> [String]
names f = take (diamonds f) $ iterate (++"prime") "temp"

--test examples
example :: LTLForm
example = LBox ((LAtom "q") `LImplies` LDiamond 20 (LAtom "p"))
example2 :: LTLForm
example2 = LBox ((LAtom "q") `LImplies` LDiamond 20 (LBox $ LAtom "p"))
example3 :: LTLForm
example3 = LBox ((LAtom "q") `LAnd` LDiamond 20 (LBox $ LAtom "p"))

d1 :: LTLForm
d1 = LDiamond 20 (LAtom "p")
d2 :: LTLForm
d2 = LDiamond 20 (LAtom "q" `LImplies` LAtom "p")
d3 :: LTLForm
d3 = LDiamond 25 $ LDiamond 20 $ LAtom "p"
d4 :: LTLForm
d4 = LAtom "q" `LImplies` (LDiamond 20 $ LAtom "r")
-- Much more to do here with actually getting this more cohesive, but right now just print each TLA+ Formula to console.
-- Need to still emit the addition of the variables and the fairness constraint (should just be WF_(NextP)? Have not thought that part out in depth)

--parseString :: String -> Either (ParseErrorBundle String String) LTLForm
parseString :: String -> IO LTLForm
parseString input = 
    case parse parseLTL "(PBLTL formula)" input of
      Left e -> ioError (userError $ "failed to parse with error " ++ show e)
      Right f -> pure f
main :: IO ()
main = do
    args <- getArgs
    form <- if null args then parseString =<< getContents else parseString =<< (readFile $ head args)
    putStrLn  "Condition:"
    print $ compileCondition form
    putStrLn  "NextP:"
    print $ collateAssignments $ compileNext form
    --This should not be primed, that's a bug. TODO.
    putStrLn  "InitP:"
    print $ collateAssignments $ compileInit form
    pure ()
