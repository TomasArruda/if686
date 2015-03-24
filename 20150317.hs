vendas :: Int -> Int
vendas n = n + 1

fun :: Int -> Int -> Int
fun s n |n == 0 && vendas 0 == s = 0
		|n == 0 = 0
		|vendas n == s = 1 + fun s (n-1)
 		|otherwise = 0 + fun s (n-1)



