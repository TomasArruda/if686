quicksort:: [Int] -> [Int]
quicksort [] = []
quicksort (pivot:list)
	|pivot > head list = (head list):(quicksort (pivot:(tail list)))
	|otherwise = pivot:(quicksort (tail list))
	