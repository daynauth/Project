import Data.List
import Data.Ratio
import System.Random
import Control.Monad
import Control.Applicative
import System.Environment
import Poly


type Vec = [Ratio Integer]
type Matrix = [Vec]

sMatrix :: Poly -> Poly -> Matrix
sMatrix u v = sHMatrix u (degreeGPE v) ++ sHMatrix v (degreeGPE u)

sHMatrix :: Poly -> Int -> Matrix
sHMatrix p n = [replicate x 0 ++ r ++ replicate (d - x) 0 | x <- [0 .. d]]
                where r = reverse p
                      d = n - 1

determinant :: Matrix -> Coef
determinant [[x]] = x
determinant mat = sum [let x = (head mat) !! i; s = (-1)^i in s*x*(determinant (getRest i mat)) | i <- [0..n-1]]
                where n = length $ head mat

getRest :: Int -> Matrix -> Matrix
getRest i mat = removeCols i (tail mat)

removeCols :: Int -> Matrix -> Matrix
removeCols _ [] = []
removeCols i (r:rs) = (left ++ (tail right)) : removeCols i rs
                where n = length r
                      (left, right) = splitAt i r

resultant' :: Poly -> Poly -> Coef
resultant' u v = determinant $ sMatrix u v

main = print $ resultant' [1,2,3,345,67,345,345,343,484,6884,4,7,88,78895, 4554,548,68,487,693,-4854,2,3] [4554,548,68,487,693,-4854,2,3,1,2,3,345,67,345,345,343,484,6884]
