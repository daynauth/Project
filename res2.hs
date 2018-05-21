import Data.List
import Data.Ratio
import System.Random
import Control.Monad
import Control.Applicative


degreeGPE [] = 0
degreeGPE p = if (last p == 0)
                then degreeGPE $ init p
                else length p - 1

sMatrix u v = sHMatrix u (degreeGPE v) ++ sHMatrix v (degreeGPE u)

sHMatrix p n = [replicate x 0 ++ r ++ replicate (d - x) 0 | x <- [0 .. d]]
                where r = reverse p
                      d = n - 1

determinant :: Integral a => [[Ratio a]] -> Ratio a
determinant [[x]] = x
determinant mat = sum [let x = (head mat) !! i; s = (-1)^i in s*x*(determinant (getRest i mat)) | i <- [0..n-1]]
                where n = length $ head mat

getRest :: Integral a => Int -> [[Ratio a]] -> [[Ratio a]]
getRest i mat = removeCols i (tail mat)

removeCols :: Integral a => Int -> [[Ratio a]] -> [[Ratio a]]
removeCols _ [] = []
removeCols i (r:rs) = [r !! j | j <- [0..n-1], j /= i] : removeCols i rs where n = length r

resultant' :: Integral a => [Ratio a] -> [Ratio a] -> Ratio a
resultant' u v = determinant $ sMatrix u v

generateR :: Int -> IO [Int]
generateR n = do
  gen <- newStdGen
  return $ take n $ randomRs (1, 100) gen

combine (x:xs) [] = x % 1 : combine xs []
combine [] p = []
combine (x:xs) (y:ys) = x % y : combine xs ys


generatePoly n = do
  x <- generateR n
  y <- generateR n
  let p = combine x y
  return p

main = do
  p1 <- generatePoly 3
  p2 <- generatePoly 2
  let r1 = resultant' p1 p2
  print r1
