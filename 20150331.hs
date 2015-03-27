--Trabalho

--Questão 1
--Haskell não aceitar o tipo sobrecarga de, por exemplo, c++ pois em c++ a função com o mesmo nome pode ser
--decalarada com parametros diferentes, ja em haskell a declaração da função é imutavel, os tipos dos 
--parametros só tem uma declaração, assim como a quantidade deles, mas é possível definir comportamentos
--diferentes para diferentes valores dos argumentos, coisa que não é possível em, por exemplo, C++. Quando
--os tipos não numa função haskell não estão declarados, estão usando uma declaração generica,
--a sobrecarga de classes em haskell resolve isso.

--Questão 2

--lookAndSay :: (Num t) => t -> Int
--lookAndSay 1 = 1


--Questão 3

data Graph a = Graph [a] [(a, a)] deriving (Show, Eq)

contens::(Eq a) => [a] -> a -> True
contens [] a = False
contens (l:ls) a
	| l == a = True
	| otherwise = contens ls a

getAdjacent :: (Eq a) => [(a,a)] -> a -> [a]
getAdjacent [] a = []
getAdjacent ((x,y):ls) a
	|x == a = (y:getAdjacent ls a)
	|y == a = (x:getAdjacent ls a)
	|otherwise =  getAdjacent ls a

excludePassBy :: (Eq a) => [a] -> a -> [a]
excludePassBy [] a = []
excludePassBy (a:as) b
	| a == b = excludePassBy as b
	|otherwise = (a:excludePassBy as b)

findBool :: (Eq a) => Graph a -> [a] -> a -> Bool
findBool (Graph [] _) _ _ = False
findBool _ [] _ = False
findBool (Graph l ls )  (x:xs) a 
	| getAdjacent ls x == [] = findBool (Graph l ls ) xs a
	| contens (getAdjacent ls x) a == True
	| otherwise = findBool (Graph (excludePassBy x) ls) (getAdjacent x) a 

find :: (Eq a) => Graph a -> [a] -> a -> [a]
find (Graph [] _) _ _ = []
find _ [] _ = []
find (Graph l ls )  (x:xs) a 
	| getAdjacent ls x == [] = find (Graph l ls ) xs a
	| contens (getAdjacent ls x) a == True = [(x,a)] 


search :: (Eq a) => Graph a -> a -> a -> [(a,a)]
search (Graph [] _) a b = []
search (Graph l ls)) a b = find (Graph (excludePassBy l) ls) (getAdjacent a) b







--search :: Graph a -> a -> a -> [a]
--search [] _ _ = []
--search (a:as) r r