module FillerAlgo where

import SimpId
import SimpFreeGroup

{- 
 - This code is supposed to model the filler algorithm for simplicial groups
 - We assume we are given a map y : \Lambda^n_i \to G where 
 -   G is a simplicial group
 -   \Lambda^n_i is a horn, which is an n-simplex with one face and the inner part removed
 -}


--   We represent such horns y by a Nat i to indicate which face is missing and then a list of n-1 group elements
--   called $ y \circ d_0 , \cdots, \hat{ y \circ d_i} \cdots y\circ d_n$. (the hat means it is ommitted)

type Horn = (Nat, [SimpGroupElt])

toHorn :: String -> Int -> Int -> Horn
-- toHorn y n i is how we represent the sieve on an n-simplex with the i'th face missing, and name y.
toHorn y n i = (i , ys) where 
                 ys = [ [N (y, [D k])] |k <- ([0..i-1] ++ [i+1..n])]

normalHorn :: Horn -> Horn 
normalHorn (i,ys) = (i, map normalformGroupElt ys)

-- We can multiply such a horn with a group element. This multiplication goes facewise.
multiplyHorn :: Horn -> SimpGroupElt -> Horn
multiplyHorn (i,ys) x = (i, zs) where 
                          xs = [compose x [D k] | k <-([0..i-1] ++ [i+1..length(ys)+1]) ]
                          zs= [multiply y x | (y,x)<- zip ys xs]

-- This algorithm works by induction in an unusual order 
-- first we start at the top and go down untill we reach the missing face (called the i'th face)
-- then we continue at 0 and go up 

suc :: Nat -> Nat -> Nat
-- S :([n] -\{i\}) -> [n]
suc i k | k >  i + 1 = k-1
      | k == i + 1 = 0
      | k <i = k+1

-- Furthermore because we make a distinction to what happens before i and after i we want the ' notation
-- which in haskell we call prime

prime :: Nat -> Nat -> Nat
prime i k | k < i = k
          | k > i = k - 1
          | otherwise = error "i' should not be called anywhere, but it is called here"

-- The k'th face of a horn can now be called as follows:
takeFace :: Horn -> Nat -> SimpGroupElt
takeFace (i, ys) k = ys !! prime i k
-- Note that in our abuse of notation for a horn we write y \circ d_k \equiv ("y", [D k]) == takeFace y k here. 

takeGap :: Horn -> Nat
takeGap (i,_) = i

takelength:: Horn -> Nat
takelength (_,ys) = length ys

-- The algorithm consists of repeating one step allong the successor function defined above 
-- It uses a helper map tau, which gives a simplex of level n (which is a group element)

tau :: Horn -> Nat -> SimpGroupElt
tau (i, ys) k = compose ( takeFace (i,ys) k ) ([ S ( prime i k)])

algoStep :: (Horn, Nat) -> (Horn, Nat) 
----this is the formula for y^{S(k}}
algoStep (y, k) = ( multiplyHorn y (inverse $ tau y k) , suc (takeGap y) k )


sol :: (Horn, Nat) -> SimpGroupElt
sol (y, k) | k == takeGap y = unit
           | otherwise = multiply ( sol(algoStep (y,k)) ) (tau y k )

fil :: Horn -> SimpGroupElt
fil y = sol (y, suc (takeGap y) ((takelength y) + 1) ) 
-- not just takelenght y, but the successor of that + 1 is technically where we start

-- below are some methods used to study the solutions

studyElt :: SimpGroupElt -> Nat -> [SimpGroupElt] 
--This code prints all faces of an n-simplex
-- The nat is needed because in our method, a group element does not carry 
--     in which layer of the simplicial group it is an element
--     or, equivalently, it's domain.  Maybe doing the theory from domains is also very useful. 
studyElt x n = [normalformGroupElt $ compose x [ D k ] | k <- [0..n]]

studyFaces :: Horn -> [SimpGroupElt]
studyFaces (_,ys) = ys

toElement :: String -> SimpGroupElt 
toElement f =  [ N (f, [])]
