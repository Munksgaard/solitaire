-- Module to shuffle a sequence.

module Shuffle
( shuffle
) where

import System.Random

randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)

make_rs :: RandomGen g => Int -> g -> ([Int],g)
make_rs n g = loop [] n g
  where
  loop acc 0 g = (reverse acc,g)
  loop acc n g = let (r,g') = randomR (0,n) g 
		 in loop (r:acc) (pred n) g'

-- A complete binary tree, of leaves and internal nodes.
-- Internal node: Node card l r
-- where card is the number of leaves under the node.
-- Invariant: card >=2. All internal tree nodes are always full.
data Tree a = Leaf a | Node !Int (Tree a) (Tree a) deriving Show

-- Convert a sequence (e1...en) to a complete binary tree
build_tree = grow_level . (map Leaf)
    where
    grow_level [node] = node
    grow_level l = grow_level $ inner l
	     
    inner [] = []
    inner x@[_] = x
    inner (e1:e2:rest) = (join e1 e2) : inner rest
	     
    join l@(Leaf _)       r@(Leaf _)       = Node 2 l r
    join l@(Node ct _ _)  r@(Leaf _)       = Node (ct+1) l r
    join l@(Leaf _)       r@(Node ct _ _)  = Node (ct+1) l r
    join l@(Node ctl _ _) r@(Node ctr _ _) = Node (ctl+ctr) l r

{-
-- example:
Main> build_tree ['a','b','c','d','e']
Node 5 (Node 4 (Node 2 (Leaf 'a') (Leaf 'b'))
               (Node 2 (Leaf 'c') (Leaf 'd')))
       (Leaf 'e')

-}

-- given a sequence (e1,...en) to shuffle, and a sequence
-- (r1,...r[n-1]) of numbers such that r[i] is an independent sample
-- from a uniform random distribution [0..n-i], compute the
-- corresponding permutation of the input sequence.

shuffle1 :: [a] -> [Int] -> [a]
shuffle1 elements rseq = shuffle1' (build_tree elements) rseq
    where
    shuffle1' (Leaf e) [] = [e]
    shuffle1' tree (ri:r_others) = extract_tree ri tree 
				    (\tree -> shuffle1' tree r_others)
	     -- extract_tree n tree
	     -- extracts the n-th element from the tree and returns
	     -- that element, paired with a tree with the element
	     -- deleted (only instead of pairing, we use CPS)
	     -- The function maintains the invariant of the completeness
	     -- of the tree: all internal nodes are always full.
	     -- The collection of patterns below is deliberately not complete.
	     -- All the missing cases may not occur (and if they do,
	     -- that's an error.
    extract_tree 0 (Node _ (Leaf e) r) k = e:k r
    extract_tree 1 (Node 2 l@Leaf{} (Leaf r)) k = r:k l
    extract_tree n (Node c l@Leaf{} r) k =
	extract_tree (n-1) r (\new_r -> k $ Node (c-1) l new_r)
    extract_tree n (Node n1 l (Leaf e)) k | n+1 == n1 = e:k l
				       
    extract_tree n (Node c l@(Node cl _ _) r) k
	| n < cl = extract_tree n l (\new_l -> k $ Node (c-1) new_l r)
	| otherwise = extract_tree (n-cl) r (\new_r -> k $ Node (c-1) l new_r)

shuffle :: [a] -> IO [a]
shuffle elements = do
  gen <- newStdGen
  return (shuffle1 elements (fst $ make_rs (length elements - 1) gen))
