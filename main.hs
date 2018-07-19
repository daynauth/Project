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



-- to modify
sMatrix u v = sHMatrix u (degreeGPE v) ++ sHMatrix v (degreeGPE u)


sHMatrix p n = [replicate x 0 ++ r ++ replicate (d - x) 0 | x <- [0 .. d]]
                where r = reverse p
                      d = n - 1

--determinant :: Integral a => [[Ratio a]] -> Ratio a
determinant [[x]] = x
determinant mat = sum [let x = (head mat) !! i; s = (-1)^i in s*x*(determinant (getRest i mat)) | i <- [0..n-1]]
                where n = length $ head mat


parDeterminant [[x]] = x
parDeterminant mat = sum [let x = (head mat) !! i; s = (-1)^i in s*x*(determinant (getRest i mat)) | i <- [0..n-1]]
                where n = length $ head mat


--getRest :: Integral a => Int -> [[Ratio a]] -> [[Ratio a]]
getRest i mat = removeCols i (tail mat)

--removeCols :: Integral a => Int -> [[Ratio a]] -> [[Ratio a]]
removeCols _ [] = []
removeCols i (r:rs) = [r !! j | j <- [0..n-1], j /= i] : removeCols i rs where n = length r



foldT f [] = []
foldT f xs = 
      let n = length xs
          (ys, zs) = splitAt (n `div` 2) xs
          left = foldT f ys
          right = foldT f zs
          in left `f` right


--resultant' :: Integral a => [Ratio a] -> [Ratio a] -> Ratio a
resultant' u v = determinant $ sMatrix u v
main = print $ resultant [1,2,3,345,67,345,345,343,484,84,4,2,3, 11,1,5,-5,8,9,3,6,9,0,4,-4,7,88,1,2,3,345,343,1,2,3,34,4,2,3,1,2,3,345,11,4,5,6,7,-1,4,56,1,5,-5,8,9,3,6,9,0,4, 4,7,88,1,2,3,345,67,345,345,343,1,2,3,345,67,345,345,343,484,84,4,2,3, 11,4,5,6,7,-1,4,56,-4,7,88,1,2,3,345,4,2,3, 11,4,5,6,7,-1,4,56,-4,7,88,895, 4554,548,68,487,693,-4854,2,3, 11,4,5,6,7,-1,4,56,-4,5,3,56,7,78895] [4554,548,68,4,2,3,56,-4,7,88,1,2,3,345,67,345,484,84,4,2,3, 11,4,5,6,7,-1,4,56,-4,7,88,895,693,-4854,2,3, 11,4,5,6,7,-1,4,56,-4,5,3,56,7,78895, 4554,548,68,2,3, 11,4,5,6,7,-1,4,56,-4,5,3,56,7,3,5,6,2,4,5,6,1,45,6,78,3,-3,4 ,11,4,5,6,7,-1,4,56,-4,7,88,1,2,3,345,3, 11,4] 


