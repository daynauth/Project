{-# LANGUAGE BangPatterns #-}

import Poly
import Data.List
import Data.Ratio


remSequence :: Poly -> Poly -> Poly
remSequence [] [] = []
remSequence r [] = r
remSequence !r !v
                | degreeGPE rn < degreeGPE v = rn
                | otherwise = remSequence rn v
                where rn = minusPoly r (multiplyBy fract xn)
                      xn = multiplyByXn v expn
                      fract = lceGPE r/lceGPE v
                      expn = fromIntegral(degreeGPE r - degreeGPE v)

resultant :: Poly -> Poly -> Coef
resultant !u !v = if (n == 0)
                then (firstPoly v) ^ m
                else if (isZero r)
                                then 0
                                else (-1) ^ (m * n) * l ^ ( m - s) * resultant v r
                where n = degreeGPE v
                      m = degreeGPE u
                      r = remSequence u v
                      s = degreeGPE r
                      l =  lceGPE v

main = print $ resultant [1,2,3,345,67,345,345,343,484,6884,4,7,88,78895, 4554,548,68,487,693,-4854,2,3] [4554,548,68,487,693,-4854,2,3,1,2,3,345,67,345,345,343,484,6884]
