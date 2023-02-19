module SimpFreeGroup where

{- This code is supposed to model the behaviour of a simplicial group
 - We model the simplicial group as follows:
    - We start of with some basic elements at each level. (In this case strings)
    - We can apply group morphisms to these basic elements, 
      these are the degeneracy and face maps (called generators)
    - We then allow for multiplication and taking inverse
-}

import SimpId

data Element a = N a | I a deriving (Eq, Show)
-- An atomof a free group is a generator in either its Inverted form or Non-inverted form
-- (N used to stand for normal, but that is also overloaded in these pieces of code)

instance Functor Element where
  fmap f ( N x ) = N (f x)
  fmap f ( I x ) = I (f x)


-- Now a free group on such atoms has as elements list of Inverted or Non-inverted atoms
-- where we can normalize such lists by 
-- letting subsequent Inverted and Non-inverted elements cancel each other out. 
normalizestep :: (Eq a) => [Element a] -> [Element a]
normalizestep ( ( N x ) : (I y) : xs ) | x == y = normalizestep xs
                                   | otherwise = ( N x ) : (normalizestep ( (I y) : xs))
normalizestep ( ( I x ) : (N y) : xs ) | x == y = normalizestep xs
                                   | otherwise = ( I x ) : (normalizestep ( (N y) : xs))

normalizestep [x] = [x]
normalizestep [] = []


-- We should iterate such normilzation steps a couple of times 
--length xs /2 + 1 should be enough steps because when we cancel, we remove 2 elements
normalize :: (Eq a ) => [Element a] -> [Element a]
normalize xs = iterate normalizestep xs !! ((div (length xs) 2) + 1)


-- now we add the generators to make it a simplicial group element. 
-- (Note there is no level specificiation, we do not specify that x\in X_n, 
-- we can get away with this for the limited application we have)
type SimpGroupElt = [Element (String , [Generators])]

-- and we give the generators normal form. 
normalgeneratorsinGroupElt :: SimpGroupElt -> SimpGroupElt
normalgeneratorsinGroupElt =  map (fmap (\(x,gs) -> (x,normalformgenerators gs)))

-- and we combine this with group normalization as seen. 
normalformGroupElt :: SimpGroupElt -> SimpGroupElt
normalformGroupElt = normalize . normalgeneratorsinGroupElt

-- This is how we compose with more generators. 
compose :: SimpGroupElt -> [Generators] -> SimpGroupElt
compose els gs = map (fmap (\(x,hs) -> (x,hs ++ gs ))) els

multiply :: SimpGroupElt -> SimpGroupElt -> SimpGroupElt
multiply = (++) 
-- At this moment, this allows us to multiply two elements from different groups 
-- because we have no level specified.
-- For example f and f\circ d1 can be multiplied in the code, 
-- but these are not elements of the same group. 
-- Again we can get away with it for the limited application. 

-- also we want to have inverses
inverseElement :: Element a -> Element a
inverseElement (N a) = I a
inverseElement (I a) = N a

inverse :: SimpGroupElt -> SimpGroupElt
inverse = reverse . (map inverseElement)

-- and an unit
unit :: SimpGroupElt 
unit = []

-- We now have composition with generators (so application of G on any function in \Delta)
-- multiplication, inverses and unit
-- Hence we have all we want out of a simplicial group 
