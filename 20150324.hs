--mergesort / heapsort

--função para separar a lista em 2 partes com take, pegando a primeira metade da lista e drop peganod a metade final
size :: [Int] -> Int
size [] = 0
size (a:as) = 1 + size as

firstHalf :: [Int] -> Int -> [Int]
firstHalf l 0 = []
firstHalf (a:as) n = [a]++firstHalf as (n-1)

secondHalf :: [Int] -> Int -> [Int]
secondHalf l 0 = l
secondHalf (a:as) n = secondHalf as (n-1)

--
merge :: [Int] -> [Int] -> [Int]
merge [] l = l
merge l [] = l
merge (a:as) (b:bs)
	|a < b = (a:merge as (b:bs))
	|otherwise = (b:merge (a:as) bs)

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort (firstHalf l ((size l)`div`2))) (mergesort (secondHalf l ((size l)`div`2)))
