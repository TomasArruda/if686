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
type Arestas = [Int]
type No = (Int,Arestas)
type Grafo = [No]
type Stack = [No]

meuGrafo :: Grafo

meuGrafo = [(1,[2,5]),(2,[1,3,5]),(3,[2,4]),(4,[3,5,6]),(5,[1,2,4]),(6,[4])]

pilha :: Stack
pilha = []

search :: Grafo -> Int -> Int -> [Int]
search grf a b
 | a == b = [b]
 | verificaAdjacencia (getAdj grf a) b == True = a : [b]
 | otherwise = [0]

getAdj :: Grafo -> Int -> [Int]
getAdj (a:as) b
 | (a:as) == [] = []
 | b == fst a = snd a
 | otherwise = getAdj as b

naoMarcado :: [Int] -> [Int] -> Int
naoMarcado [] (a:as) = a
naoMarcado marcado [] = 0
naoMarcado marcado (a:as)
 | elem a marcado = naoMarcado marcado as
 | otherwise = a

verificaAdjacencia :: Arestas -> Int -> Bool
verificaAdjacencia [] b = False
verificaAdjacencia (a:as) b
 | (a:as) == [] = False
 | b == a = True
 | b /= a = verificaAdjacencia as b
 | otherwise = False

dfs :: Grafo -> [Int] -> [Int] -> [Int]
dfs grafo marcado [] = reverse marcado
dfs grafo marcado (a:as) 
 | elem a marcado = dfs grafo marcado as --pega o proximo da lista de adj nao marcado
 | otherwise = dfs grafo (a:marcado) (getAdj grafo a) --marca o cara e chama a funcao passando os filhos dele

dfs2 :: Grafo -> [Int] -> Int -> [Int] -> [Int]
dfs2 grafo1 marcado1 no [] = reverse marcado1
dfs2 grafo1 marcado1 no (a:as) 
 | no == 0 = dfs2 grafo1 marcado1 (naoMarcado marcado1 (getAdj grafo1 (head as))) as -- se chegou numa folha ou num cara q ja tem todos os vizinhos marcados, da um pop na pilha e chama novamente a funcao
 | no /= 0 = dfs2 grafo1 (no:marcado1) (naoMarcado marcado1 (getAdj grafo1 no)) (no:marcado1)  -- marca o cara, coloca ele na pilha e chama a funcao com os adjacentes a ele
 | otherwise = []



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
	
	


	
--Exercícios de sala

getNext :: [(Int,Int,String)] -> Int -> Char -> String
getNext [] _ _ = []
getNext ((v1,v2,res):as) estado c
	|v1 ==  estado && (res !! 0) == c = show v2
	|otherwise = getNext as estado c
	
contem :: [Int] -> Int -> Bool
contem [] _ = False
contem (a:as) x 
	|a == x = True
	|otherwise = contem as x

afd :: String -> [Int] -> [(Int,Int,String)] -> Int -> [Int] -> Bool
afd [] _ _ inicial aceitacao  = contem aceitacao inicial
afd (a:entradas) estados regras inicial aceitacao = afd entradas estados regras (read (getNext regras inicial a)) aceitacao

hexChar :: Char -> Int
hexChar c
    | c == '0' = 0
    | c == '1' = 1
    | c == '2' = 2
    | c == '3' = 3
    | c == '4' = 4
    | c == '5' = 5
    | c == '6' = 6
    | c == '7' = 7
    | c == '8' = 8
    | c == '9' = 9
    | c == 'A' = 10
    | c == 'B' = 11
    | c == 'C' = 12
    | c == 'D' = 13
    | c == 'E' = 14
    | c == 'F' = 15
    | otherwise = 0
toDecimal :: String -> Int
toDecimal x 
    | length x /= 0 = (hexChar(last(x)))+(16*toDecimal(init(x)))
    | otherwise = 0 

toHexadecimal :: Int -> String
toHexadecimal 0 = []
toHexadecimal n
	| n `mod` 16 == 0 = toHexadecimal (n `div` 16) ++ "0"
	| n `mod` 16 == 1 = toHexadecimal (n `div` 16) ++ "1"
	| n `mod` 16 == 2 = toHexadecimal (n `div` 16) ++ "2"
	| n `mod` 16 == 3 = toHexadecimal (n `div` 16) ++ "3"
	| n `mod` 16 == 4 = toHexadecimal (n `div` 16) ++ "4"
	| n `mod` 16 == 5 = toHexadecimal (n `div` 16) ++ "5"
	| n `mod` 16 == 6 = toHexadecimal (n `div` 16) ++ "6"
	| n `mod` 16 == 7 = toHexadecimal (n `div` 16) ++ "7"
	| n `mod` 16 == 8 = toHexadecimal (n `div` 16) ++ "8"
	| n `mod` 16 == 9 = toHexadecimal (n `div` 16) ++ "9"
	| n `mod` 16 == 10 = toHexadecimal (n `div` 16) ++ "A"
	| n `mod` 16 == 11 = toHexadecimal (n `div` 16) ++ "B"
	| n `mod` 16 == 12 = toHexadecimal (n `div` 16) ++ "C"
	| n `mod` 16 == 13 = toHexadecimal (n `div` 16) ++ "D"
	| n `mod` 16 == 14 = toHexadecimal (n `div` 16) ++ "E"
	| n `mod` 16 == 15 = toHexadecimal (n `div` 16) ++ "F"


somaHexadecimal :: String -> String -> String
somaHexadecimal a b = toHexadecimal ((toDecimal a)+(toDecimal b))

somatorioHexadecimal :: [String] -> String
somatorioHexadecimal [] = "0"
somatorioHexadecimal (a:as) = somaHexadecimal a (somatorioHexadecimal as)

inverter :: String -> String
inverter [] = []
inverter (a:as) = (inverter as)++[a]

stringIgual :: String -> String -> Bool
stringIgual [] (a:[]) = False
stringIgual (a:[]) [] = False
stringIgual [] [] = True
stringIgual (a:as) (b:bs)
	| a == b = stringIgual as bs
	|otherwise = False

palindromoCheck :: String -> Bool
palindromoCheck [] = False
palindromoCheck s = stringIgual a (inverter b)
	where 
		a = take ((length s) `div` 2) s
		b = drop ((length s) `div` 2) s

palindromo :: String -> String
palindromo [] = []
palindromo s 
	|palindromoCheck h == False = h++" - NAO-PALINDROMO"
	|otherwise = h++" - PALINDROMO"
	where h = show (toDecimal s)

type Vector = [Double]
type Matrix = [Vector]

multiplicaMatrizes :: Matrix -> Matrix -> Matrix
multiplicaMatrizes [] [] = []



