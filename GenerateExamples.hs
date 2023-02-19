module GenerateExamples where

{- This module was used to check whether with our definitions 
 - for effective Kan complexes and simplicial groups work out such that
 - simplicial groups are effective Kan complexes. 
 - If it does, wrongcases should always return an empty list.
 -
 - This code was used to play around with examples of freely generated groups. 
 - It was used to find the correct formulation of the pulled back horn in Hornsquares.hs
 - That formulation is now correct, and hence this code does not give new information. 
-}

import SimpId
import SimpFreeGroup
import FillerAlgo
import HornSquares

-- the valid combinations for i*,j* given i,j
validcombination :: Nat ->  Nat  -> [(Nat, Nat)]
validcombination i j | j < i = [(i+1,j)]
                     | j > i = [(i,j+1)]
                     | j == i = [(i+1,i),(i,i+1)] 
                     | otherwise = error "no ijcomparison"

-- the set of i,j,i*,j* combinations given some n
mathcalA :: Nat -> [(Nat,Nat,Nat,Nat)]
mathcalA n = [ (i,j,i+1,j)| i<-[0..n],j<-[0..i] ] ++ 
             [ (i,j,i,j+1)| i<-[0..n],j<-[i..n] ] ++ 
             [ (i,i,i,i+1)| i<-[0..n]] ++ 
             [ (i,i,i+1,i)| i<-[0..n]]

-- we can check that for a certain horn square, we have that the 
-- effective condition holds:
effectiveCheck :: PM -> Nat -> Nat -> Nat -> Nat -> Nat -> Bool
effectiveCheck pm n i j i' j'= 
    normalformGroupElt solz == normalformGroupElt expectedsolz where
          y = toHornSquare "y" pm n i 
          z = pullBackHorn "y" pm n i j i' j'
          solz = fil (squareToHorn z )
          expectedsolz = compose (fil (squareToHorn (toHornSquare "y" pm n i))) [S j']

-- now we generate given n all possible horn squares and apply the check above
lotsofcases :: Nat -> [(Bool, PM, Nat, Nat, Nat , Nat )] 
lotsofcases n = [(effectiveCheck pm n i j i' j',pm,i,j, i', j') | 
                    pm <- [Min,Plus], (i,j,i',j') <- mathcalA n]

-- now we filter when this goes wrong 
wrongcases :: Nat -> [(Bool, PM, Nat, Nat, Nat ,Nat)] 
wrongcases n = filter (not.first) (lotsofcases n) where first (b, _,_,_, _ , _) = b
