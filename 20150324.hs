--Trabalho do dia

--mergesort / heapsort

--mergesort
--A complexidade do firstHalf e do secondHalf é n/2, do size é n, do merge é n (sendo n o tamanho da maior lista), por sua vez o mergesort tem complexidade n log n 
size :: [Int] -> Int
size [] = 0
size (a:as) = 1 + size as

firstHalf :: [Int] -> Int -> [Int]
firstHalf l 0 = []
firstHalf (a:as) n = [a]++firstHalf as (n-1)

secondHalf :: [Int] -> Int -> [Int]
secondHalf l 0 = l
secondHalf (a:as) n = secondHalf as (n-1)

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


--heapsort
--o heapsort está com complexidade n log n
trocar :: Int -> Int -> [Int] -> [Int]
trocar i j xs 
 | i == j    = xs
 | i < j = firstElements xs i ++ ( get xs j) : firstElements (lastElements xs (i+1)) (j-i-1) ++ (get xs i) : lastElements xs (j+1)
 | otherwise = firstElements xs j ++ ( get xs i) : firstElements (lastElements xs (j+1)) (i-j-1) ++ (get xs j) : lastElements xs (i+1)
 
maior :: Int -> Int -> [Int] -> Int
maior i hs xs -- decide o maior entre o pai e seus dois filhos e retorna o indice do maior
 | ((2 * i + 1) < hs) && ((get xs (2 * i + 1)) > (get xs i)) = maiorAux (2 * i + 1) i hs xs -- se o filho 'pertence' a lista e ele é maior que o pai entao deve trocar 
 | otherwise = maiorAux i i hs xs -- se nao o pai é maior
        
 
maiorAux :: Int -> Int -> Int -> [Int] -> Int -- a partir da comparacao do filho da esquerda e o pai compara o maior deles com o filho da direita e retorna decisao final
maiorAux b i hs xs -- decide o maior entre o pai e seus dois filhos e retorna o indice do maior
 | ((2 * i + 2) < hs) && ((get xs (2 * i + 2)) > (get xs b)) = (2 * i + 2) -- se o filho 'pertence' a lista e ele é maior que o pai entao deve trocar 
 | otherwise = b

heapify :: Int -> Int -> [Int] -> [Int]
heapify i hs lista  
 | ((maior i hs lista) /= i) = heapify (maior i hs lista) hs (trocar (maior i hs lista) i lista)
 | otherwise = lista
        
 
buildheap :: Int -> [Int] -> [Int]
buildheap 0 lista = heapify 0 (size lista) lista --chegou na raiz, daí retorna a nova lista com o raiz do heap no final da lista
buildheap n lista = buildheap (n - 1) (heapify n (size lista) lista) -- recursivamente vai comparando o valor dos filhos

heapsort :: [Int] -> [Int]
heapsort lista = heapsortAux (size lista - 1) (buildheap (size lista `div` 2) lista) -- começa o algoritmo no meio da lista
              
heapsortAux :: Int -> [Int] -> [Int]
heapsortAux i lista
 | i /= 1 = heapsortAux (i - 1) (heapify 0 i (trocar 0 i lista)) -- 
 | otherwise = (heapify 0 i (trocar 0 i lista))
 

get :: [Int] -> Int -> Int
get (a:as) 0 = a
get (a:as) n = get as (n-1)

firstElements :: [Int] -> Int -> [Int]
firstElements list 0 = []
firstElements (a:as) n = [a] ++ firstElements as (n-1)

lastElements :: [Int] -> Int -> [Int]
lastElements list 0 = list
lastElements (a:as) n = lastElements as (n-1)



--Exercício da aula

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

fib::Int->[Int]
fib 1 = [1]
fib 2 = [1, 1]
fib n = (head (fib (n-1)) + head (fib (n-2))) : fib (n-1)

justEven:: [Int] -> [Int]
justEven [] = []
justEven (a:as)
	| a `mod` 2 == 0 = (a:justEven as)
	| otherwise = justEven as

evenFib:: Int -> [Int]
evenFib n = justEven (quicksort (fib n))

menorMaior :: Int -> Int -> Int -> (Int,Int)
menorMaior a b c
	|a <= b && a <= c && b <= c = (a,c)
	|a <= b && a <= c && b >= c = (a,b)
	|b <= a && b <= c && a <= c = (b,c)
	|b <= a && b <= c && a >= c = (b,a)
	|c <= a && c <= b && a <= b = (c,b)
	|otherwise = (c,a)


ordenaTripla :: (Int,Int,Int) -> (Int,Int,Int)
ordenaTripla (a,b,c)
	|a <= b && a <= c && b <= c = (a,b,c)
	|a <= b && a <= c && b >= c = (a,c,b)
	|b <= a && b <= c && a <= c = (b,a,c)
	|b <= a && b <= c && a >= c = (b,c,a)
	|c <= a && c <= b && a <= b = (c,a,b)
	|otherwise = (c,b,a)

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

firstCoor :: Ponto -> Float
firstCoor (a,b) = a

secondCoor :: Ponto -> Float
secondCoor (a,b) = b

isVertical :: Reta -> Bool
isVertical (pont1,pont2)
	| firstCoor pont1 == firstCoor pont2 = True
	| otherwise = False

pontoY :: Float -> Reta -> Float
pontoY x ((x1,y1), (x2,y2)) = ((y2-y1/x2-x1)*(x-x1))+y1




