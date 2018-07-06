{-# LANGUAGE BangPatterns #-}

import Poly
import Data.List
import Data.Ratio
import Control.Parallel 
import Control.Parallel.Strategies 
import Control.DeepSeq



parMultiplyBy :: Coef -> Poly -> Poly
parMultiplyBy a p1 = parMap rpar (a*) p1 


remainder :: Poly -> Poly -> Poly
remainder [] [] = []
remainder r [] = r
remainder r v
                | degreeGPE rn < degreeGPE v = rn
                | otherwise = remainder rn v
                where rn = minusPoly r (parMultiplyBy fract xn)
                      xn = multiplyByXn v expn
                      fract = lceGPE r/lceGPE v
                      expn = fromIntegral(degreeGPE r - degreeGPE v)

--parFold f = foldl1' f . withStrategy (parList rseq)
parFold f = foldl1' f . withStrategy (parBuffer 5 rseq)

parResultant u v = (firstPoly $ last r) * parFold (*) [(-1) ^ (m * n) * l ^ (m - s) | i <- [0 .. n - 3], 
      let m = degreeGPE (r !! i); n = degreeGPE (r !! (i + 1)); s = degreeGPE (r !! (i + 2)); l = lceGPE (r !! (i + 1))]
       where r = remSeq u v
             n = length r




remSeq :: Poly -> Poly -> [Poly]
remSeq u v
    | degreeGPE u <= 0 = [u]
    | otherwise = [u] ++ (remSeq v r)
      where r = remainder u v




data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)


instance NFData a => NFData (Tree a) where
  rnf EmptyTree = ()
  rnf (Node a left right) = rnf a `seq` rnf left `seq` rnf right


singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree


treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
      | x <= a  = Node a (treeInsert x left) right
      | x > a  = Node a left (treeInsert x right)


remTree tree !u !v = if degreeGPE v <= 0
                   then treeInsert (firstPoly v) tree
                   else remTree (treeInsert cal tree) v r
                   where cal = (-1) ^ (m * n) * l ^ (m - s)
                         n = degreeGPE v
                         m = degreeGPE u
                         s = degreeGPE r
                         l = lceGPE v
                         r = remainder u v

tFold f (EmptyTree) = 1
tFold f (Node x left right) = x `f` (tFold f left) `f` (tFold f right)


parTreeFold f (EmptyTree) = 1
parTreeFold f (Node x left right) = (leftTree) `par` ((rightTree) `pseq` result)
  where leftTree = parTreeFold f left
        rightTree = parTreeFold f right
        result = x `f` leftTree `f` rightTree


--parResultant' u v = parTreeFold (*) (remTree EmptyTree u v)
parResultant' u v = parTreeFold (*) (remTree EmptyTree u v) 



--resultant' :: Integral a => [Ratio a] -> [Ratio a] -> Ratio a
main = print $ parResultant' [1,2,3,345,67,345,345,343,484,84,4,2,3, 11,1,5,-5,8,9,3,6,9,0,4,-4,7,88,1,2,3,345,343,1,2,3,34,4,2,3,1,2,3,345,11,4,5,6,7,-1,4,56,1,5,-5,8,9,3,6,9,0,4, 4,7,88,1,2,3,345,67,345,345,343,1,2,3,345,67,345,345,343,484,84,4,2,3, 11,4,5,6,7,-1,4,56,-4,7,88,1,2,3,345,4,2,3, 11,4,5,6,7,-1,4,56,-4,7,88,895, 4554,548,68,487,693,-4854,2,3, 11,4,5,6,7,-1,4,56,-4,5,3,56,7,78895] [4554,548,68,4,2,3,56,-4,7,88,1,2,3,345,67,345,484,84,4,2,3, 11,4,5,6,7,-1,4,56,-4,7,88,895,693,-4854,2,3, 11,4,5,6,7,-1,4,56,-4,5,3,56,7,78895, 4554,548,68,2,3, 11,4,5,6,7,-1,4,56,-4,5,3,56,7,3,5,6,2,4,5,6,1,45,6,78,3,-3,4 ,11,4,5,6,7,-1,4,56,-4,7,88,1,2,3,345,3, 11,4] 

