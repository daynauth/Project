import Data.List
import Data.Ratio
import System.Random
import Control.Monad
import Control.Applicative
import System.Environment
import Poly
import Control.Parallel 
import Control.Parallel.Strategies 
import Control.DeepSeq

sMatrix :: Poly -> Poly -> Matrix
sMatrix u v = sHMatrix u (degreeGPE v) ++ sHMatrix v (degreeGPE u)

sHMatrix :: Poly -> Int -> Matrix
sHMatrix p n = [replicate x 0 ++ r ++ replicate (d - x) 0 | x <- [0 .. d]]
                where r = reverse p
                      d = n - 1

parSMatrix :: Poly -> Poly -> Matrix
parSMatrix u v = runEval $ do 
      as <- rpar(sHMatrix u (degreeGPE v))
      bs <- rpar(sHMatrix v (degreeGPE u))
      rseq as
      rseq bs
      return (as ++ bs)

determinant :: Matrix -> Coef
determinant [[x]] = x
determinant mat = sum [let x = (head mat) !! i; s = (-1)^i in s*x*(determinant (getRest i mat)) | i <- [0..n-1]]
                where n = length $ head mat

parDeterminant :: Matrix -> Coef
parDeterminant mat = sum cofactor
                  where cofactor = [(-1)^i * x * determinant (getRest i mat) |  (i, x) <- zip [0.. n - 1] (head mat)] `using` parList rdeepseq
                        n = length $ head mat

getRest :: Int -> Matrix -> Matrix
getRest i mat = removeCols i (tail mat)

removeCols :: Int -> Matrix -> Matrix
removeCols _ [] = []
removeCols i (r:rs) = (left ++ (tail right)) : removeCols i rs
                where n = length r
                      (left, right) = splitAt i r


parResultant' :: Poly -> Poly -> Coef
parResultant' u v = parDeterminant $ sMatrix u v

resultant' :: Poly -> Poly -> Coef
resultant' u v = determinant $ parSMatrix u v


main = print $ parResultant' [84,84,4,2,1,5,3,4, 3] [4,7,4,5,5,4]--main = print $ parResultant' [1,2,3,345,67,345,345,343,484,6884,4,7,88,78895, 4554,548,68,487,693,-4854,2,3] [4554,548,68,487,693,-4854,2,3,1,2,3,345,67,345,345,343,484,6884]
