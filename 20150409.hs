--Trabalho
--Questão 1
data Graph a = Graph [a] [(a, a, Int)] deriving (Show, Eq, Ord)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (pivot:list) = (quicksort (smaller list pivot))++[pivot]++ (quicksort (bigger list pivot))

quicksortTupla :: [Int] -> [Int]
quicksortTupla [] = []
quicksortTupla (pivot:list) = (quicksortTupla (smaller list pivot))++[pivot]++ (quicksortTupla (bigger list pivot))

compareList :: [a] -> [a] -> Bool
compareList [] [] = True
compareList (a:as) (b:bs)
	|a == b = compareList as bs
	|otherwise = False

compareListTupla :: [(a,a,Int)] -> [(a,a,Int)] -> Bool
compareList [] [] = True
compareList ((a1,a2,p1):as) ((b1,b2,p2):bs)
	| a1 == b1 && a2 == b2 && p1 == p2 = compareListTupla as bs
	|otherwise = False

equals :: (Graph a) -> (Graph a) -> Bool
equals (Graph l1 la1) (Graph l2 la2)
	|compareList (quicksort l1) (quicksort l2) == True = compareListTupla la1 la2
	|otherwise = False

--Questão 2


--Exercícios