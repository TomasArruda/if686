--trabalho
--questão1

compose :: (Int->Int) -> [(Int->Int)] -> [(Int -> Int)]
compose func funcs = [(composeAux func f)|f<-funcs]

composeAux :: (Int->Int) -> (Int->Int) -> Int -> Int
composeAux func1 func2 v = func1 (func2 v)

--questão2
data Graph a = Graph [a] [(a, a, Int)] deriving (Show, Eq, Ord)

mapGraph :: (a->a) -> (Graph a) -> (Graph a)
mapGraph _ (Graph [] _ ) =  (Graph [] [])
mapGraph func (Graph ls lls) = (Graph [(func v)|v<-ls] [((func v1),(func v2),v3)|(v1,v2,v3)<-lls])

foldGraph :: (a->a->a) -> (Graph a) -> a -> a
foldGraph func (Graph [] _ ) base =  base
foldGraph func (Graph (x:xs) l) base =  func x (foldGraph func (Graph xs l) base)

--questão3
data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

filterTree :: (Eq t) => (t->Bool) -> (Tree t) -> [(Tree t)]
filterTree _ NilT = []
filterTree func node = [x]++(allList func xs)
	where (x,xs) = filterTreeAux2 func node

allList ::(Eq t) => (t->Bool) -> [(Tree t)] -> [(Tree t)]
allList _ [] = []
allList func (a:as)
	|x /= NilT = (x:(allList func as))++(allList func xs)
	|otherwise = (allList func as)++(allList func xs)
	where (x,xs) = filterTreeAux2 func a

filterTreeAux2 :: (Eq t)=> (t->Bool) -> (Tree t) -> ((Tree t),[(Tree t)])
filterTreeAux2 _ NilT = (NilT, [])
filterTreeAux2 func (Node v n1 n2)
	| func v == False && n1 == NilT && n2 == NilT = (NilT, [])
	| func v == False && n1 == NilT = (NilT, [n2])
	| func v == False && n1 == NilT = (NilT, [n1])
	| func v == False = (NilT, [(n1),(n2)])
	| otherwise =  (Node v x y,xs++ys)
	where 
		(x,xs) = filterTreeAux2 func n1
		(y,ys) = filterTreeAux2 func n2

--exerciciosel

--elimina :: Int -> [[Int]] -> [[Int]]
--elimina valor ls  = [l|l<-ls, (foldr (+) 0 l)<valor]

elimina :: Int -> [[Int]] -> [[Int]]
elimina v ls  = filter (\x->(foldr (+) 0 x)>=v) ls

inter ::(Eq t) => [t] -> [t] -> [t]
inter l1 l2 = []

diff ::(Eq t) => [t] -> [t] -> [t]
inter l1 l2 = []
