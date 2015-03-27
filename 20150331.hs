--Trabalho

--Questão 1
--Haskell não aceitar o tipo sobrecarga de, por exemplo, c++ pois em c++ a função com o mesmo nome pode ser
--decalarada com parametros diferentes, ja em haskell a declaração da função é imutavel, os tipos dos 
--parametros só tem uma declaração, assim como a quantidade deles, mas é possível definir comportamentos
--diferentes para diferentes valores dos argumentos, coisa que não é possível em, por exemplo, C++. Quando
--os tipos não numa função haskell não estão declarados, estão usando uma declaração generica,
--a sobrecarga de classes em haskell resolve isso.

--Questão 2
lookAndSay ::  Int -> String
lookAndSay n
	|n == 1 = "1"
	|otherwise = juntar (lookAndSay (n-1))

repete :: String -> Int
repete [] = 0
repete (a:[]) = 1
repete (a:b:as)
	|a==b = 1+repete(b:as)
	|otherwise = 1

concatena :: String -> (Int, Char, String)
concatena s = (repete s, fst temp, snd temp )
	where temp = resto s

resto :: String -> (Char,String)
resto  [] = ('0', [])
resto (a:[]) = (a, [])
resto (a:b:as)
	|a == b = resto (b:as)
	|otherwise = (a,(b:as))


juntar :: String -> String
juntar [] = []
juntar s = (show x)++[y]++(juntar z)
	where (x,y,z) = concatena s

--Questão 3

data Graph a = Graph  [(a, [a])] deriving (Show, Eq)






--Questão 4

--mediana :: [Int] -> Int
--media ls
--	|length 




--search :: Graph a -> a -> a -> [a]
--search [] _ _ = []
--search (a:as) r r