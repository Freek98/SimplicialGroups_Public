module SimpId where

{- This code is supposed to help calculate simplicial identities
 - In order to do so, we introduce the face and degeneracy maps
 - And we write some code to calculate the normal form of such maps
 - and equality of compositions of such maps
 -
 - the data type of Generators model the face and degeneracy maps
 - the function normalformgenerators calculates the normal form of a list of such maps, 
 - read as composition
-}


type Nat = Int

-- We define the maps as functions Nat -> Nat.
-- It should however be noted that these maps come with a domain and codomain 
-- (d i : [n] -> [n+1], s i : [n+1]\to [n]) 
-- We do not include these as it makes the code easier to write.
-- 
-- It might help to do sanity-checks in which we do keep track of the domain and codomain. 
-- But for the purposes of the thesis I only want to do quick calculations. 

-- definition of face maps
d :: Nat -> Nat -> Nat
d  i  x | x < i     = x
        | otherwise = x + 1

-- definition of degeneracy maps
s :: Nat -> Nat -> Nat
s  i  x | x <= i     = x
        | otherwise = x - 1

--how to show maps [n] -> [m], and check for equality between those maps, given a domain [n]. 
shown :: Nat -> (Nat -> Nat) -> String
shown n f = show( map (\x ->(x, f x)) [0..n])

check :: Nat -> (Nat -> Nat) -> (Nat -> Nat) -> Bool
check n f g = (shown n f) ==  (shown n g)

-- We introduce abstract notions of degeneracy and face maps, which are not functions. 
-- We call these Generators as they generate all maps in the simplex category. 
data Generators = D Nat | S Nat deriving (Eq, Show)

-- We then introduce a way to go from a list of generators to their function composition.
tomap :: Generators -> (Nat -> Nat)
tomap (D n) = d n
tomap (S n) = s n 

concatter :: [a -> a] -> a -> a
concatter []     = id
concatter (x:xs) = x . (concatter xs)

lstomap :: [Generators] -> (Nat -> Nat)
lstomap gs = concatter(map tomap gs)

-- Now it can be shown that every map in \Delta (every increasing map [n] \to [m])
-- can be written as, d_i \circ \cdots \circ d_j \circ s_k \circ \cdots \circ s_l
-- If we furthermore require that the s_i are taken in decreasing order, 
-- and the d_i in increasing order, 
-- this form is unique. We use this normal form to help us study simplicial identities.

helpnormalformer :: Nat -> Nat ->  [Generators] ->  Nat -> (Nat -> Nat) -> [Generators]
helpnormalformer input expectedoutput gs n f  
-- We assume that f is an increasing function and that 
-- on [0.. input -1], the function f equals the composition of the generators gs
-- this function takes that assumption and add generators untill this also holds on [0..input]
-- untill we reach input > n.
--
-- First we check if we have reached n already
  | input > n = gs
-- Now we keep track of an expectedoutput, which will be the output the generators will give. 
-- If this is indeed what f also gives, we continue, 
  | f input == expectedoutput = helpnormalformer (input + 1 ) (expectedoutput + 1) gs n f 
-- If f gives a bigger output, it skips over the epected output.
-- skipping over a number corresponds to D 
  | f input >  expectedoutput = helpnormalformer input (expectedoutput + 1) 
                                                ((D expectedoutput):gs) n f
-- If f gives a lower output, it can only be equal to expectedoutput -1
-- Because by assumption f (input - 1) = expectedoutput -1 and f is increasing
-- If this is the case, we double on expectedoutput-1, 
-- doubling over a number corresponds to S
  | f input == expectedoutput - 1 = helpnormalformer (input+1) (expectedoutput) 
                                        (gs ++ [S (input - 1) ]) n f 
-- If this does not happen, we do not have an increasing function. 
  | otherwise = error "I don't think this function is increasing."

normalformer :: Nat -> (Nat -> Nat) -> [Generators]
normalformer = helpnormalformer 0 0 []

checknf n f = check n f ( lstomap (normalformer n f))

helpnormalformgenerators :: Nat -> [Generators] -> [Generators]
helpnormalformgenerators n gs = normalformer n (lstomap gs)

-- After a while, any finite composition f of generators 
-- should satisfy (f (k+1) == f k + 1)  for all k\geq N
-- Where is this N? Well it should be at least the maximum of the indices we encounter,
-- and all other maps could make it one bigger, so maximum + length is a good estimate, 
-- but we add 2 to be sure. 

normalformgenerators :: [Generators] -> [Generators]
normalformgenerators gs = helpnormalformgenerators maxn gs where 
                            dismantle :: Generators -> Nat
                            dismantle (D n) = n
                            dismantle (S n) = n 
                            ns = map dismantle gs
                            maxn | ns /= [] = (length gs) + (maximum ns) +  2
                                 | ns == [] = 2







