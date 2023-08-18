module Stats where
import Statistics.Distribution.Binomial
import Statistics.Distribution
import GHC.Float (int2Double)

cdf :: Int -> Int -> Double -> Double
cdf c n p =  cumulative (binomial n p) (int2Double c)

inverseBinomial :: Double -> Int -> Double -> Int
inverseBinomial y n p 
  | y == 1 = n
  | y == 0 = -1
  | p == 0 = 0
  | p == 1 = n
  | otherwise = 
      if (cdf low n p) >= y then low else high
        where
            getBounds :: Int -> Int -> (Int,Int)
            getBounds high low = 
                if low == 0 || high - low <= 1 || (cdf low n p) < y
                   then (high,low)
                   else getBounds low (low `div` 2) 
            search :: (Int,Int) -> (Int,Int)
            search (high,low) = 
                if high - low <= 1 
                   then (high,low)
                   else if (cdf middle n p) >= y 
                        then search (middle,low)
                        else search (high,middle)
                    where middle = (high + low) `div` 2
            (high,low) = search $ getBounds n (ceiling ((int2Double n) / 2))

samplingPlan :: Double -> Double -> Double -> Double -> (Int,Int)
-- higher prob. bound for hyp. rejected, lower prob. bound on hyp. accepted
-- acceptable rate of false positives, acceptable rate of false negatives
samplingPlan p0 p1 a b = 
         answer $ search $ getBounds 1 0
        where
            getBounds :: Int -> Int -> (Int,Int)
            getBounds high low = 
                if c0 >= c1
                   then (high, low)
                   else  getBounds (high*2) high
                       where 
                           c0 = high - (inverseBinomial (1-a) high (1-p0)) - 1
                           c1 = inverseBinomial (1-b) high p1
            search :: (Int,Int) -> (Int,Int)
            search (high,low) = 
                if high - low <= 1 
                    then (high,low) 
                    else if c0 < c1 
                        then search (high,mid) 
                        else search (mid,low) 
                       where 
                           c0 = mid - (inverseBinomial (1-a) mid (1-p0)) - 1
                           c1 = inverseBinomial (1-b) mid p1
                           mid = ceiling $ (int2Double (high + low))/2
            answer :: (Int,Int) -> (Int,Int)
            answer (high,low) = 
                (high,(c0+c1) `div` 2)
                    where
                        c0 = high - (inverseBinomial (1-a) high (1-p0)) - 1
                        c1 = inverseBinomial (1-b) high p1
