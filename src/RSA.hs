module RSA (createFunctions, RSASet(Set, pub, priv, border, prod, privInt, pubInt), 
			prims, createSet) where

	import System.IO.Unsafe
	import System.Random
	import Data.List.Split

	data RSASet = Set { pub :: (Integer->Integer)
						,priv :: (Integer->Integer)
						, prod :: Integer
						, border :: Integer
						, pubInt ::Integer
						, privInt ::Integer }  --das quadrat der 50. Primzahl 

	instance Show RSASet where
		show (Set _ _ prod border pubInt privInt ) = (show prod) ++ " " ++ (show border) ++ " " ++ (show pubInt) ++ " " ++ (show privInt)

	
	readFromString::String -> RSASet
	readFromString str = Set (dec e n) (dec d n) n border e d 
				where e = read (strlist !! 2)  ::Integer;
					  d = read (strlist !! 3) ::Integer;
					  border = read (strlist !! 1) ::Integer;
					  n = read (strlist !! 0 )::Integer;
					  strlist = splitOn " " str

	createSet :: Integer -> Integer -> Integer -> Integer -> RSASet
	createSet e d n border = Set (dec e n) (dec d n) n border e d 

	prims :: Integer -> [Integer]
	prims zahl = innerPrims [2..zahl]
				where innerPrims (x:xs) = x : innerPrims [ p | p<- xs, (p`mod`x) /= 0];
					  innerPrims [] = []

	pq :: [Integer] -> (Integer, Integer)
	pq prims = if p==q then pq prims else (p, q)
				where p= prims !! unsafePerformIO(randomRIO(50, (length prims)));
					  q= prims !! unsafePerformIO(randomRIO(51 , (length prims)))

	phiN :: (Integer, Integer) -> Integer
	phiN prims = ((fst prims) - 1) * ((snd prims) - 1)

	genN :: (Integer, Integer) -> Integer
	genN prims = ((fst prims) ) * ((snd prims) )

	ggt:: Integer -> Integer -> Integer
	ggt b 0= b
	ggt a b = ggt lt (gt `rem` lt)
			where gt = max a b;
				  lt = min a b

	genE:: Integer-> Integer
	genE phi = if ggt a phi == 1 then a else genE phi
			where a = unsafePerformIO (randomRIO(3, phi ) )

	genD :: Integer -> Integer -> Integer
	genD e phi = inverseMod e phi

		--innerd 2
		--	where innerd d = if d * e `rem` phi == 1 then d else innerd (d+1)  

	dec :: Integer -> Integer -> Integer -> Integer
	dec e n t = t ^ e `rem` n

	a // b = innerM a b 1
			where innerM a d i = if a - (d*i) > 0 
									then innerM a d (i+1)
									else i 

	inverseMod e phi =
	  (x + phi) `mod` phi
	  where
	    (x, y) = euclid e phi

	-- extended euclidean algorithm
	euclid :: Integer -> Integer -> (Integer, Integer)
	euclid 0 _ = (0,1)
	euclid _ 0 = (1,0)
	euclid e n = (t, s-q*t)
	    where
	      (q, r) = quotRem e n;
		  (s, t) = euclid n r

	createFunctions :: RSASet --(Integer->Integer,Integer->Integer)
	createFunctions = (Set (dec e n) (dec d n) n 54289 e d) ----das quadrat der 50. Primzahl (54289) 
			where pr = pq $ prims 10000;
				  r = phiN pr;
				  n =  genN pr; 
				  e = genE r;
				  d = genD e r;