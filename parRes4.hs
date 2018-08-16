{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE BangPatterns #-}

import Poly
import Data.List
import Data.Ratio
import Control.Parallel 
import Control.Parallel.Strategies 
import Control.DeepSeq



parMultiplyBy :: Coef -> Poly -> Poly
parMultiplyBy a p1 = parMap rpar (a*) p1 

parMinusPoly :: Poly -> Poly -> Poly
parMinusPoly p1 p2 = parAddPoly p1 (parMultiplyBy (-1) p2)

parAddPoly :: Poly -> Poly -> Poly
parAddPoly p1 p2 = if (length p1 >= length p2)
                then [x + y | x <- p1 | y <- (p2 ++ repeat 0)]
                else parAddPoly p2 p1



remainder :: Poly -> Poly -> Poly
remainder [] [] = []
remainder r [] = r
remainder !r !v = remainder' n n r v
          where n = length r



--remainder' :: Int -> Poly -> Poly -> Poly
remainder' _ _ [] [] = []
remainder' n k r [] = r
remainder' n k !r !v
                | degreeGPE rn < degreeGPE v = rn
                | otherwise = if k > (n `div` 2)  
                                 then rem `par` rem
                                 else rem
                where rn = parMinusPoly r (parMultiplyBy fract xn)
                      xn = multiplyByXn v expn
                      fract = lceGPE r/lceGPE v
                      expn = fromIntegral(degreeGPE r - degreeGPE v)
                      rem = remainder' n (k - 1) rn v




--parFold f = foldl1' f . withStrategy (parList rseq)
parFold f = foldl1' f . withStrategy (parBuffer 5 rdeepseq)

parResultant u v = (firstPoly $ last r) * parFold (*) [(-1) ^ (m * n) * l ^ (m - s) | i <- [0 .. n - 3], 
      let m = degreeGPE (r !! i); n = degreeGPE (r !! (i + 1)); s = degreeGPE (r !! (i + 2)); l = lceGPE (r !! (i + 1))] 
       where r = remSeq u v
             n = length r

chunk _ [] = []
chunk n xs = y1 : chunk n y2
      where (y1, y2) = splitAt n xs


remSeq :: Poly -> Poly -> [Poly]
remSeq u v
    | degreeGPE u <= 0 = [u]
    | otherwise = [u] ++ (remSeq v r)
      where r = remainder u v

foldT f [] = 1      
foldT f (x:[]) = x
foldT f xs = 
      let n = length xs
          (ys, zs) = splitAt (n `div` 2) xs
          left = foldT f ys
          right = foldT f zs
          in left `par` (right `pseq` (left `f` right))


foldT' f [] = 1      
foldT' f (x:[]) = x
foldT' f xs = 
      let n = length xs
          (ys, zs) = splitAt (n `div` 2) xs
          in runEval $ do
              left <- rpar (force(foldT' f ys))
              right <- rpar (force (foldT' f zs))
              rseq left
              rseq right
              return (left `f` right)



--resultant' :: Integral a => [Ratio a] -> [Ratio a] -> Ratio a
main = print $ parResultant [1,2,3,345,67,345,345,343,484,84,4,2,3, 11,1,5,-5,8,9,3,6,9,0,4,-4,7,88,1,2,3,345,343,1,2,3,34,4,2,3,1,2,3,345,11,4,5,6,7,-1,4,56,1,5,-5,8,9,3,6,9,0,4, 4,7,88,1,2,3,345,67,345,345,343,1,2,3,345,67,345,345,343,484,84,4,2,3, 11,4,5,6,7,-1,4,56,-4,7,88,1,2,3,345,4,2,3, 11,4,5,6,7,-1,4,56,-4,7,88,895, 4554,548,68,487,693,-4854,2,3, 11,4,5,6,7,-1,4,56,-4,5,3,56,7,78895] [4554,548,68,4,2,3,56,-4,7,88,1,2,3,345,67,345,484,84,4,2,3, 11,4,5,6,7,-1,4,56,-4,7,88,895,693,-4854,2,3, 11,4,5,6,7,-1,4,56,-4,5,3,56,7,78895, 4554,548,68,2,3, 11,4,5,6,7,-1,4,56,-4,5,3,56,7,3,5,6,2,4,5,6,1,45,6,78,3,-3,4 ,11,4,5,6,7,-1,4,56,-4,7,88,1,2,3,345,3, 11,4] 


