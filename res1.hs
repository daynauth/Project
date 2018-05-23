import Data.List
import Data.Ratio
import System.Random
import Control.Monad
import Control.Applicative
import System.Environment


multiplyBy a p1 = map (a*) p1

addPoly p1 p2 = if (length p1 >= length p2)
                then zipWith (+) p1 (p2 ++ repeat 0)
                else addPoly p2 p1

minusPoly p1 p2 = addPoly p1 (multiplyBy (-1) p2)

multiplyByX p = 0:p

degreeGPE [] = 0
degreeGPE p = if (last p == 0)
                then degreeGPE $ init p
                else length p - 1

lceGPE [] = 0
lceGPE p = if (last p == 0)
                then lceGPE $ init p
                else last p

multiplyByXn p n = take n (cycle [0]) ++ p

firstPoly :: Integral a => [Ratio a] -> Ratio a
firstPoly [] = 0
firstPoly p = head p

isZero p = p == take n (cycle [0])
                where n = length p

t1 = [1,4,5] :: [Ratio Integer]
t2 = [3,2] ::  [Ratio Integer]

remSequence :: Integral a => [Ratio a] -> [Ratio a] -> [Ratio a]
remSequence [] [] = []
remSequence r [] = r
remSequence r v
                | degreeGPE rn < degreeGPE v = rn
                | otherwise = remSequence rn v
                where rn = minusPoly r (multiplyBy fract xn)
                      xn = multiplyByXn v expn
                      fract = lceGPE r/lceGPE v
                      expn = fromIntegral(degreeGPE r - degreeGPE v)

resultant u v = if (n == 0)
                then (firstPoly v) ^ m
                else if (isZero r)
                                then 0
                                else (-1) ^ (m * n) * l ^ ( m - s) * resultant v r
                where n = degreeGPE v
                      m = degreeGPE u
                      r = remSequence u v
                      s = degreeGPE r
                      l =  lceGPE v

generateR :: Int -> IO [Int]
generateR n = do
                gen <- newStdGen
                return $ take n $ randomRs (1, 10) gen

convertToRatio = map (%1)

generatePoly n = do
                  x <- generateR n
                  let p = convertToRatio x
                  return p

main = do (a:_) <- getArgs
          let n = read a :: Int
          p1 <- generatePoly n
          p2 <- generatePoly n
          let r1 = resultant p1 p2
          print r1
