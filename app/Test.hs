module Test where
import Stats
import Test.QuickCheck (choose, Gen)
import Test.QuickCheck.Property

generateSampleProblem :: Gen (Double,Double,Double,Double)
generateSampleProblem = do
    p0 <- choose (0,1)
    -- p1 < p0
    -- does sometimes fail for too small of a difference between p1 and p0 (seemingly due to float impercision), but holds for this
    p1 <- choose (0,max 0.0 (p0-0.00001))
    -- and and b can't be 0, or too close to it due to floating point error
    a  <- choose (0.00000000001,p0/10)
    b  <- choose (0.00000000001,p1/10)
    pure (p0,p1,a,b)

testSampleProblem :: (Double,Double,Double,Double) -> Bool
testSampleProblem (p0,p1,a,b) =
    let (n,c) = samplingPlan p0 p1 a b in
        (cdf c n p0) <= a && (1 - (cdf c n p1)) <= b

-- holds for a million tests.
prop_isTruePlan :: Gen Result
prop_isTruePlan = do
    v <- generateSampleProblem
    pure $ if testSampleProblem v 
       then succeeded
       else failed { reason = "samplePlan failed with input " ++ show v}
