module StringMod (decStr, encStr) where

prims  = innerPrims [2..233]
			where innerPrims (x:xs) = x : innerPrims [ p | p<- xs, (p`mod`x) /= 0];
				  innerPrims [] = []

concatCharInt = zip (' ' : ['a'..'z']) prims

concatIntChar = zip prims (' ' : ['a'..'z'])

filterFromList:: Eq a => [a] -> [(a,b)] -> [b]
filterFromList [] _ = []
filterFromList (f:frst) scnd = innerfiler f scnd : filterFromList frst scnd
			where innerfiler f (s:sd) = if f == (fst s) 
											then snd s
											else innerfiler f sd

toProducts :: [Integer] -> [Integer]
toProducts xs
			| l >= 2 = if possible
						then (product (take 2 xs )) : toProducts (drop 2 xs)
						else (xs !! 0)^2 : toProducts (drop 1 xs )
			| l == 1 = xs !! 0 : toProducts (drop 1 xs )
			| otherwise = []
			where l = length xs;
				  possible = (xs !! 0) < (xs !! 1)

toInts :: [Integer] -> [Integer]
toInts [] = []
toInts (x:xlst) = [p | p <- prims, (rem x p ) == 0 ] ++ toInts xlst

decStr::String -> [Integer]
decStr str = toProducts $ filterFromList str concatCharInt

encStr::[Integer] -> String
encStr prods = filterFromList (toInts prods) concatIntChar