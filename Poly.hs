module Poly(
Poly,
Coef,
multiplyBy,
addPoly,
minusPoly,
multiplyByXn,
degreeGPE,
lceGPE,
firstPoly,
isZero,
) where

import Data.List
import Data.Ratio

type Coef = Ratio Integer
type Poly = [Coef]

multiplyBy :: Coef -> Poly -> Poly
multiplyBy a p1 = map (a*) p1

addPoly :: Poly -> Poly -> Poly
addPoly p1 p2 = if (length p1 >= length p2)
                then zipWith (+) p1 (p2 ++ repeat 0)
                else addPoly p2 p1

minusPoly :: Poly -> Poly -> Poly
minusPoly p1 p2 = addPoly p1 (multiplyBy (-1) p2)

multiplyByXn :: Poly -> Int -> Poly
multiplyByXn p n = take n (cycle [0]) ++ p

degreeGPE :: Poly -> Int
degreeGPE [] = 0
degreeGPE p = if (last p == 0)
                then degreeGPE $ init p
                else length p - 1

lceGPE :: Poly -> Coef
lceGPE [] = 0
lceGPE p = if (last p == 0)
                then lceGPE $ init p
                else last p

firstPoly :: Poly -> Coef
firstPoly [] = 0
firstPoly p = head p

isZero :: Poly -> Bool
isZero p = p == take n (cycle [0])
                where n = length p
