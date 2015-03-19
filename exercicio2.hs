double :: [Int] -> [Int]
double (a:as)
	| as == [] = (a * 2:[])
	| otherwise = (2*a:double as)

member :: [Int] -> Int -> Bool
member (a:as) number
	| a == number = True
	| as == [] = False
	| otherwise = member as number

digits :: String -> String
digits [] = []
digits (a:as)
	| a == '1' = (a:digits as)
	| a == '2' = (a:digits as)
	| a == '3' = (a:digits as)
	| a == '4' = (a:digits as)
	| a == '5' = (a:digits as)
	| a == '6' = (a:digits as)
	| a == '7' = (a:digits as)
	| a == '8' = (a:digits as)
	| a == '9' = (a:digits as)
	| a == '0' = (a:digits as)
	|otherwise = digits as

--digits :: String -> String
--digits (a:as)
--	| a >= '0' && a <= '9' && as == [] = (a:[])
--	| a >= '0' && a <= '9' = (a:digits as)
--	|otherwise = digits as

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs (a:as) (b:bs)
	| as == [] && bs == [] = (a+b:[])
	| as == [] = bs
	| bs == [] = as
	| otherwise = (a+b: sumPairs as bs)