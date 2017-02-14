-- calcula el factorial de un numero
fact:: Int->Int
fact n | n==0 = 1
       | n>0 = n*fact(n-1)

-- calcula los años que pasaron entre 2 fechas
edad :: Int -> Int -> Int -> Int -> Int -> Int -> Int
edad x y z j k l | y < k = abs(l-z)
		 | y > k = abs(l-z-1)
		 | (y == k) && (x <= j) = abs(l-z)
		 | (y == k) && (x > j) = abs(l-z-1)

--Saca la cantidad de divisores entre 2 numeros
cantdiv :: Int -> Int -> Int
cantdiv n m | m == 1 = 1
	    | m > 0 && mod n m == 0  =  1 +  cantdiv n (m-1)  
            | m > 0 && mod n m /= 0 = cantdiv n (m -1)

--te dice si un numero es primo o no
esprimo ::Int -> Bool
esprimo n = (cantdiv n n) == 2

--te dice si un numero es primo o no (lista por comprencion)
esprimo2 n = length[x | x <- [1..n], mod n x == 0] == 2

--dado un numero, retorna una lista de todos los numeros primos menores que el
todoslosprimos :: Int -> [Int]
todoslosprimos x | x == 1 = []
		 | x > 1 && esprimo x = todoslosprimos (x-1)++[x]	
		 | x >= 1 && not(esprimo x) = todoslosprimos (x-1)

-- Da vuelta todos los elementos de una lista (polimorfica)
reversa:: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa (xs) ++ [x]	

--Dada dos listas devuelve true, si estas son iguales
iguales:: (Eq a) => [a] -> [a] -> Bool
iguales [] [] = True
iguales [x] [y] = x == y
iguales (x:xs) (y:ys) | length(x:xs) == length(y:ys) && x == y = iguales xs ys
		      | not( length(x:xs) == length(y:ys) && x /= y) = False

		
-- Dada 1 lista devuelve true, si es palindromo (capicua) y false en caso contrario
palindrome :: [Int] -> Bool
palindrome [] = True
palindrome [x] = True
palindrome(x:xs) = iguales (reversa (x:xs)) (x:xs) 

--dada dos listas ordenasdas, devuelve una ordenada
concatenarordenadas:: [Int] -> [Int] -> [Int]
concatenarordenadas [] [] = []
concatenarordenadas [] (y:ys) = (y:ys)
concatenarordenadas (x:xs) [] = (x:xs)
concatenarordenadas (x:xs) (y:ys) | x < y = [x] ++ concatenarordenadas (xs) (y:ys) 
				  | x > y = [y] ++ concatenarordenadas (x:xs) (ys) 
				  | x == y = [x] ++ concatenarordenadas (xs) (ys) 

--dado un n, calcula 2^2
dosalan:: Int -> Int
dosalan n | n==0 = 1
	  | n==1 = 2
          | n>1 = 2* dosalan(n-1)

--Dado un numero, retorna su reprecentacion binaria
numbinario:: Int -> [Int]
numbinario x | x==1 = [1]
	     | mod x 2 == 1 = numbinario (div x 2) ++ [1]
	     | mod x 2 == 0 = numbinario (div x 2) ++ [0]

--dado un numero binario nos dice si es par o no
binarioespar:: Int -> Bool
binarioespar x = last (numbinario x)==0

--dado un numero, nos dice si es cuadrado perfecto o no
cuadradoperfecto:: Int-> Bool
cuadradoperfecto x = length [n | n <- [1..div x 2], n^2==x] == 1

--dada un elemento y una lista cuanta cuantas veces esta el elemento en la lista
elemlista::(Eq a) =>  a -> [a] -> Int
elemlista a [] = 0
elemlista a [x] | x == a = 1
	        | x /= a = 0
elemlista a (x:xs) | a == x = 1 + elemlista a xs
		   | a /= x = 0 + elemlista a xs

borra :: Eq a => a -> [a] -> [a]
borra x [] = []
borra x (y:ys) | x == y = ys
	       | otherwise = y : borra x ys

--dadas 2 listas, nos dice si una es permutacion de la otra o no
espermutacion :: Eq a => [a] -> [a] -> Bool
espermutacion [] [] = True
espermutacion [] (y:ys) = False
espermutacion (x:xs) ys = elem x ys && espermutacion xs (borra x ys)

matches:: Int -> [Int] -> [Int]
matches n [] = []
matches n [x] | n == x = [x]
	      | n /= x = []
matches n (x:xs) | n == x = [x] ++ matches n xs
		 | n /= x = matches n xs

--nos da los divisores de un numero
divisores:: Int -> [Int]
divisores n = [ x | x <- [1..n-1], mod n x == 0]

--nos dice si un numero es perfecto o no
numperfecto:: Int -> Bool
numperfecto x = sum (divisores x) == x

g:: Int -> [Int] -> Bool
g y [] = y == 0
g y (x:xs) = y == sum(x:xs) || g x xs

f:: [Int] -> Bool	  
f xs = g 0 xs

--dada una lista nos dice si hay repetidos o no
norep:: Eq a => [a] -> Bool
norep [] = True
norep [x] = True
norep (x:xs) | (elem x xs == True) = False
	     | (elem x xs == False) = norep xs
