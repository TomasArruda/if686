import Data.Char (ord)

--Trabalho
--Questão 1
data Graph a = Graph [a] [(a, a, Int)] deriving (Show, Eq, Ord)

quicksort :: (Ord t) => [t] -> [t]
quicksort [] = []
quicksort (p:ls) = quicksort([e1|e1<-ls, e1<=p])++[p]++quicksort([e2|e2<-ls, e2>p])

quicksortTupla :: (Ord t) => [(t,t,Int)] -> [(t,t,Int)]
quicksortTupla [] = []
quicksortTupla (p:ls) = quicksortTupla([r|r<-ls, r<=p])++[p]++quicksortTupla([t|t<-ls, t>p])

compareList :: (Ord a) => [a] -> [a] -> Bool
compareList [] [] = True
compareList (a:as) (b:bs)
	|a == b = compareList as bs
	|otherwise = False

compareListTupla :: (Ord a) => [(a,a,Int)] -> [(a,a,Int)] -> Bool
compareListTupla [] [] = True
compareListTupla ((a1,a2,p1):as) ((b1,b2,p2):bs)
	| a1 == b1 && a2 == b2 && p1 == p2 = compareListTupla as bs
	|otherwise = False

equals :: (Ord a) => (Graph a) -> (Graph a) -> Bool
equals (Graph l1 la1) (Graph l2 la2)
	|compareList (quicksort l1) (quicksort l2) == True = compareListTupla (quicksort la1) (quicksort la2)
	|otherwise = False


--Questão 2

contens::(Eq a) => [a] -> a -> Bool
contens [] a = False
contens (l:ls) a
	| l == a = True
	| otherwise = contens ls a

getAdjacent :: (Eq a) => [(a,a,Int)] -> a -> [a]
getAdjacent [] a = []
getAdjacent ((x,y,p):ls) a
	|x == a = (y:getAdjacent ls a)
	|y == a = (x:getAdjacent ls a)
	|otherwise =  getAdjacent ls a

	
excludePassBy :: (Eq a) => [a] -> a -> [a]
excludePassBy [] a = []
excludePassBy (a:as) b
	| a == b = excludePassBy as b
	|otherwise = (a:excludePassBy as b)

orList :: [Bool] -> Bool
orList [] = False
orList (x:xs) = x || (orList xs)

-- o grafo, lista de adjacencias ou [nó inicial], lista de nós que serão excluidos quando passados, nó destino
findBool :: (Eq a) => Graph a -> [a] -> [a] -> a -> [Bool]
findBool _ _ [] _ = [False]
findBool _ [] _ _ = [False]
findBool (Graph l ls ) (x:xs) ys a 
	| adjacentes == [] = findBool (Graph l ls ) xs ys a
	| contens (adjacentes) a == True = [True]
	| contens ys x == False =  findBool (Graph l ls ) xs ys a
	| otherwise = (False:findBool (Graph l ls) adjacentes (excludePassBy ys x) a)
	where
		adjacentes = getAdjacent ls x

find :: (Eq a) => Graph a -> a -> a -> Bool
find (Graph l ls ) ini cheg = orList (findBool (Graph l ls) [ini] l cheg)

--Exercícios



raiz a = sqrt a

--todosRaiz :: [t] -> [t]
todosRaiz [a] = map raiz [a]

posicaoAlfabeto :: Char -> Int
posicaoAlfabeto c = (ord c)-96

todosPosicaoAlfabeto :: [Char] -> [Int]
todosPosicaoAlfabeto c = map posicaoAlfabeto c 

meuMap :: (t->u) -> [t] -> [u]
meuMap func ls = [func l |l<-ls]

member :: (Eq t) => t -> [t] -> Bool
member a as = foldr (||) False (map ((==)a) as)

juntar :: (Ord t) => [t] -> [t] -> [t]
juntar [] l = l
juntar l [] = l
juntar (a:as) (b:bs)
	|(member a (b:bs)) == True = (b:(juntar as bs))
	|otherwise =(a:(juntar as (b:bs)))

union :: (Ord t) => [t] -> [t] -> [t]
union as bs = foldr (juntar) [] ([as]++[bss])

somaCharStringAux :: String -> Int
somaCharStringAux l = foldr (+) 0 (todosPosicaoAlfabeto l)

somaCharString :: [String] -> [Int]
somaCharString ls = (map somaCharStringAux ls)
	



