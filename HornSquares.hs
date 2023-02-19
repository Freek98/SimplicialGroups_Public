module HornSquares where
{- This module models horn squares in the sense of Berg and Faber
 -}

import Data.List

import SimpId
import SimpFreeGroup
import FillerAlgo


data PM = Plus | Min deriving (Eq, Show)

type HornSquare = (Nat, PM, SimpGroupElt , [SimpGroupElt])

-- the Hornsquare (i, Min, f , gs) is the square
-- 
-- let n = length gs + 1
--
-- \partial \Delta^n  ----------> s_i^*(\partial \Delta^{n+1})
--     |                                |
--     |                                |   all faces except d_i, d_i+1
--     |                                |
--    \ /                              \ /
--     .                                . 
-- \Delta^n    -------------------> \Delta^{n+1}
--                      d_i
--  and a map f from the bottom left corner to G
--  and a map g from the top right corner to G 
--  where g\circ d_k | k < i = gs !! k
--                   | k > i+2 = gs !! k-2
--   
-- and the Hornsquare (i,Plus, f, gs) changes the bottom arrow from d_i to d_{i+1}.
--
--
--We want a method to take a face of such a horn square
takesqFace :: HornSquare -> Nat -> SimpGroupElt
takesqFace (i, Min, f, gs) k | k < i = gs !! k
                             | k == i = f
                             | k == i+ 1 = error "You are trying to take a misssing face"
                             | k > i+1 = gs !! (k-2)

takesqFace (i, Plus, f, gs) k | k < i = gs !! k
                              | k == i+1 = f
                              | k == i = error "You are trying to take a misssing face"
                              | k > i+1 = gs !! (k-2)

--Some terminology on which face is the added face and which is the missing face
missingFace :: Nat -> PM -> Nat
missingFace i Min  = i+1
missingFace i Plus = i

addedFace :: Nat -> PM -> Nat
addedFace i Min  = i
addedFace i Plus = i + 1

--How to deal with these horn squares in relation to horns. 
squareToHorn :: HornSquare -> Horn
squareToHorn (i,pm,f,gs) =  (hole,simpElements) where
                  hole = missingFace i pm 
                  newface = addedFace i pm 
                  (gs1, gs2) = splitAt i gs
                  simpElements = gs1 ++ [f] ++ gs2

hornToSquare :: Horn -> PM -> HornSquare
hornToSquare (hole, hs) pm = (i,pm,f,gs) where
    i | pm == Min = hole - 1
      | pm == Plus = hole 
    (gs1,f:gs2) = splitAt i hs
    gs = gs1 ++ gs2
   
toHornSquare :: String -> PM -> Int -> Int -> HornSquare
toHornSquare y pm n i = (i, pm, f, gs) where
  f = [ N (y, [D (addedFace i pm) ]) ]
  gs = [ [N (y, [D k])] |k <- ([0..i-1] ++ [i+2..n+1])] 
  -- the horn square should end at n+1

--What does the pullback of these horn squares along s_j look like:
pullBackHorn :: String -> PM -> Nat ->  Nat -> Nat -> Nat -> Nat -> HornSquare    
pullBackHorn ys pm n i j i' j' = (i', pm, f, zs) where

-- this gives the correct pulled back horns, 
  y = squareToHorn $ toHornSquare ys pm n i 
  fily = fil y
  z :: Nat -> SimpGroupElt
  z k | (k == i' || k == i'+1 ) = error "z should not be called on i', i'+1"
      | (k == j' || k == j'+1  ) = fily 
      -- guess based on confusion about which faces we add: 
      -- we changed j to j'. 
      | otherwise =  [ N (ys, [S j' , D k])]
  zs = [ z k | k <- [0.. i'-1] ++ [i'+2 .. n+2]] 
  -- we use n+2 as the literature has n+1 horns, 
  -- so the pullbacks are n+2 horns
  f | (pm == Plus && j == i && i' == i && j'==i+1) = fily 
    | (pm == Min  && j == i && i' == i+1 && j'==i) = fily 
    | otherwise = [N(ys, [ D (addedFace i pm), S j])] 
  --the case distinction says that the added face might have fil y as value as well. 
