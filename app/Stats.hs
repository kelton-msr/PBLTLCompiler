module Stats where
import GHC.Float (int2Double)

inverseBinomial :: Int -> Double -> Double -> Int
inverseBinomial n y p 
    | y == 1 || p == 1 = n
    | y == 0 || p == 0 = 0
    | otherwise = 
        inverseBinomLoop f f 0
        where
            q = 1 - p 
            r = p / q 
            f = q^n
            inverseBinomLoop :: Double -> Double -> Int -> Int
            inverseBinomLoop f s i = 
                if s < y && i < n 
                    then
                        let f' = f * r * ((int2Double (n-i))/(int2Double (i+1))) 
                        in inverseBinomLoop f' (s + f') (i + 1)
                    else i 

-- Comically slow for high percision, but I think the original impl. is too?
samplingPlan :: Double -> Double -> Double -> Double -> (Int,Int)
-- higher prob. bound for hyp. rejected, lower prob. bound on hyp. accepted
-- acceptable rate of false positives, acceptable rate of false negatives
samplingPlan p0 p1 a b = 
    sampleLoop (-1) 0 0
    where
        sampleLoop :: Int -> Int -> Int -> (Int,Int)
        sampleLoop c0 c1 n =
            if c0 < c1 
                then sampleLoop (n -  inverseBinomial (n+1) (1-a) (1-p0)) 
                                (inverseBinomial (n+1) (1-b) p1)
                                (n+1)
                else (n,min n ((c0+c1) `div` 2))
    
