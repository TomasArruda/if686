--Trabalho

--Questão 1
--Haskell não aceitar o tipo sobrecarga de, por exemplo, c++ pois em c++ a função com o mesmo nome pode ser
--decalarada com parametros diferentes, ja em haskell a declaração da função é imutavel, os tipos dos 
--parametros só tem uma declaração, assim como a quantidade deles, mas é possível definir comportamentos
--diferentes para diferentes valores dos argumentos, coisa que não é possível em, por exemplo, C++. Quando
--os tipos não numa função haskell não estão declarados, estão usando uma declaração generica,
--a sobrecarga de classes em haskell resolve isso.

--Questão 2
lookAndSay ::(Show t) => Int -> t -> String
lookAndSay n c
	|n == 1 && (length (show c)) > 1 = [(show c) !! 1]
	|n == 1 = show c
	|otherwise = juntar (lookAndSay (n-1) c)

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

--existem vários tipos de fazer o filtro de mediana, uma forma é preenchendo as bordas com os números das bordas para poder pegar a janela
--com os pixels das bordas, outra forma é não fazer a transformação com os pixels da broda para poder pegar a janela dentro do frame,
--a terceira maneira, a usada aqui é preencher as bordas com zero para poder pegar a janela dos pixels da borda e fazer a transformação.

filtroMediana :: [[Int]] -> Int -> [[Int]]
filtroMediana [] _ = []
filtroMediana l 0 = l
filtroMediana l janelaTam = filtroMedianaLin l janelaTam 1


--gera a matriz com os valores transformados
filtroMedianaLin :: [[Int]] -> Int -> Int -> [[Int]]
filtroMedianaLin  l janelaTam posx 
	|posx > length l = []
	|otherwise = ((filtroMedianaCol l janelaTam posx 1):(filtroMedianaLin l janelaTam (posx+1)))

filtroMedianaCol :: [[Int]] -> Int -> Int -> Int ->[Int]
filtroMedianaCol l janelaTam posx posy 
	|posy > length l = []
	|otherwise = ((mediana (quicksort (emLinha (vizinhos l lMaior janelaTam posx posy 0)))):(filtroMedianaCol l janelaTam posx (posy+1)))
	where lMaior = fazerListaMaior l l janelaTam 0 0


--pegar os vizinhos na matrix grande e transformar numa lista
emLinha :: [[Int]] -> [Int]
emLinha [] = []
emLinha (a:as) = a++(emLinha as) 


--cria uma matriz com os vixinhos
vizinhos :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
vizinhos _ [] _ _ _ _ = []
vizinhos l (a:as) janelaTam posx posy posxMaior
	|posxMaior < (posx-tamBorda) = vizinhos l as janelaTam posx posy (posxMaior+1)
	|posxMaior > (posx+tamBorda) = []
	|otherwise = ((vizinhosAux l a janelaTam posy 0):(vizinhos l as janelaTam posx posy (posxMaior+1)))
	where 
		tamBorda = janelaTam`div`2
vizinhosAux :: [[Int]] -> [Int] -> Int -> Int -> Int -> [Int]
vizinhosAux _ [] _ _ _ = []
vizinhosAux l (a:as) janelaTam posy posyMaior
	|posyMaior < (posy-tamBorda) = vizinhosAux l as janelaTam posy (posyMaior+1)
	|posyMaior > (posy+tamBorda) = []
	|otherwise = (a:(vizinhosAux l as janelaTam posy (posyMaior+1))) 
	where 
		tamBorda = janelaTam`div`2


--criar uma matriz maior, com as laterais com valor = 0
fazerListaMaior :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> [[Int]]
fazerListaMaior l [] janelaTam posx posxMaior
	|posxMaior >= (tam + tamBorda + tamBorda - 1) = [(fazerListaMaiorAux l [] janelaTam posxMaior 0 0)]
	|otherwise = ((fazerListaMaiorAux l [] janelaTam posxMaior 0 0):(fazerListaMaior l [] janelaTam posx (posxMaior+1)))
	where 
		tam = length l
		tamBorda = janelaTam`div`2
fazerListaMaior l (a:as) janelaTam posx posxMaior
	|posxMaior >= (tam + tamBorda + tamBorda - 1) = [(fazerListaMaiorAux l [] janelaTam posxMaior 0 0)]
	|posxMaior < tamBorda || posxMaior >= (tam+tamBorda)|| ((a:as) == []) = ((fazerListaMaiorAux l [] janelaTam posxMaior 0 0):(fazerListaMaior l (a:as) janelaTam posx (posxMaior+1)))
	|otherwise = ((fazerListaMaiorAux l a janelaTam posxMaior 0 0):(fazerListaMaior l as janelaTam (posx+1) (posxMaior+1)))
	where 
		tam = length l
		tamBorda = janelaTam`div`2
fazerListaMaiorAux :: [[Int]] -> [Int] -> Int -> Int -> Int -> Int -> [Int]
fazerListaMaiorAux l ls janelaTam posxMaior posy posyMaior 
	|posyMaior >= (tam + tamBorda + tamBorda) = []
	|posxMaior < tamBorda || posyMaior < tamBorda = (0:fazerListaMaiorAux l ls janelaTam posxMaior posy (posyMaior+1))
	|posxMaior >= (tam + tamBorda) || posyMaior >= (tam + tamBorda)  = (0:fazerListaMaiorAux l ls janelaTam posxMaior posy (posyMaior+1))
	|otherwise = ((ls!!posy):fazerListaMaiorAux l ls janelaTam posxMaior (posy+1) (posyMaior+1))
	where 
		tam = length l
		tamBorda = janelaTam`div`2


--calcular a mediana de uma lista
mediana :: [Int] -> Int
mediana [] = 0
mediana ls = medianaCalc (quicksort ls)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (p:ls) = quicksort([e1|e1<-ls, e1<=p])++[p]++quicksort([e2|e2<-ls, e2>p])

medianaCalc :: [Int] -> Int
medianaCalc [] = 0
medianaCalc ls
	|odd (length ls) == True = ls !! (((length ls)`div`2))
	|otherwise = ((ls !! ((length ls)`div`2))+(ls !! (((length ls)`div`2)-1)))`div`2


