bigger:: [Int] -> Int -> [Int]
bigger [] p = []
bigger (a:as) p
	| a >=  p = (a: bigger as p)
	|otherwise = bigger as p

smaller:: [Int] -> Int -> [Int]
smaller [] p = []
smaller (a:as) p
	| a <  p = (a: smaller as p)
	|otherwise = smaller as p

quicksort:: [Int] -> [Int]
quicksort [] = []
quicksort (pivot:list) = (quicksort (smaller list pivot))++[pivot]++ (quicksort (bigger list pivot))

	