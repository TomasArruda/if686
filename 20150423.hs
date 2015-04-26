--Trabalo
--questão 1
--primeiro exercicio
mapfoldr :: (a1 -> a -> a) -> [a] -> [[a1] -> a]
mapfoldr func l = meuMap (meuFoldr  func ) l

meuMap :: (t->a) -> [t] -> [a]
meuMap func l = [func e|e<-l]

meuFoldr :: (a -> b -> b) -> b -> [a] -> b
meuFoldr f z [] = z
meuFoldr f z (x:xs) = f x (meuFoldr f z xs)

aplica :: [[a]->b] -> [a] -> [b]
aplica [] _ = []
aplica (f:funcs) l = ((f l):(aplica funcs l))

--segundo exercicio
bigger :: [Int] -> Int 
bigger = maximum

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

funciso :: Tree t -> (Tree t -> Bool)
funciso no = iso no

iso :: Tree t -> Tree t -> Bool
iso (Node _ (Node _ _ _) (Node _ _ _)) (Node _ (Node _ _ _) NilT) = False
iso (Node _ (Node _ _ _) (Node _ _ _)) (Node _ NilT NilT) = False
iso (Node _ (Node _ _ _) NilT) (Node _ NilT NilT) = False
iso (Node _ NilT (Node _ _ _)) (Node _ (Node _ _ _) (Node _ _ _) ) = False
iso (Node _ NilT NilT) (Node _ (Node _ _ _) (Node _ _ _) ) = False
iso (Node _ NilT NilT) (Node _ NilT (Node _ _ _) ) = False
iso (Node _ NilT (Node _ _ _)) (Node _ (Node _ _ _) NilT ) = False
iso (Node _ (Node _ _ _) NilT ) (Node _ NilT (Node _ _ _)) = False
iso (Node _ (Node _ _ _) (Node _ _ _)) (Node _ NilT (Node _ _ _) ) = False
iso (Node _ (Node _ _ _) NilT ) (Node _ (Node _ _ _) (Node _ _ _)) = False
iso (Node _ NilT NilT) (Node _ (Node _ _ _) NilT ) = False
iso (Node _ NilT (Node _ _ _) ) (Node _ NilT NilT) = False
iso (Node _ NilT NilT) (Node _ NilT NilT) = True
iso (Node _ x NilT) (Node _ y NilT) = iso x y
iso (Node _ NilT x) (Node _ NilT y) = iso x y
iso (Node _ x y) (Node _ r t) = (iso x y)&&(iso r t)


funcpares:: [a] -> ([b] -> [(a,b)])
funcpares l = pares l

pares:: [a] -> [b] -> [(a,b)]
pares [] _ = []
pares _ [] = []
pares (x:xs) (y:ys) = ((x,y):(pares xs ys))

--questão 2
data Graph a = Graph [a] [(a, a, Int)] deriving (Show, Eq, Ord)
data Tree2 t = NilT2 | Node2 t [Tree2 t] deriving (Eq, Show)

getAdjacent :: (Eq a) => [(a,a,Int)] -> a -> [a]
getAdjacent [] a = []
getAdjacent ((x,y,p):ls) a
	|x == a = (y:getAdjacent ls a)
	|y == a = (x:getAdjacent ls a)
	|otherwise =  getAdjacent ls a

adjacencias::(Eq a) => (Graph a) -> [a] -> a -> a -> (Tree2 a)
adjacencias (Graph l ls) pilha ini cheg 
	|elem ini pilha == True = NilT2
	|ini == cheg = (Node2 ini [NilT2]) 
	|otherwise = Node2 ini [(adjacencias (Graph l ls) (ini:pilha) x cheg )|x<-adjs]
		where
		adjs = getAdjacent ls ini

allPaths :: Tree2 a -> [[a]]
allPaths NilT2 = [[]]
allPaths (Node2 a ls) = [a:x | t <- ls, x <- allPaths t]

getDistance::(Eq a) =>[(a,a,Int)] -> a -> a -> Int
getDistance ((x,y,p):ls) a b 
	| (x == a) && (y == b) = p
	| (x == b) && (y == a) = p
	|otherwise = (getDistance ls a b)

allDistances :: (Eq a) =>[(a,a,Int)] -> [a] -> [Int]
allDistances ls (b:[]) = []
allDistances ls (a:b:rest) = ((getDistance ls a b):(allDistances ls (b:rest)))

distances ::(Eq a) => (Graph a) -> [[a]] -> a -> [([a],Int)]
distances (Graph l ls) paths cheg =  [(p,(foldr (+) 0 (allDistances ls p)))|p<-path]
	where path = filter (elem cheg) paths

menor :: [([a],Int)] -> ([a],Int) -> [a]
menor [] (l,dist) = l
menor ((lx,distx):ls) (l,dist)
	|distx < dist = menor ls (lx,distx)
	|otherwise = menor ls (l,dist)
	

shortest :: [([a],Int)] -> [a]
shortest [] = []
shortest ((l,dist):[]) = l
shortest ((l,dist):ls) = menor ls (l,dist)
	
geraFuncoMenorCaminho ::(Eq a) => (Graph a) -> (a->a->[a])
geraFuncoMenorCaminho graph = \x y ->shortest (distances graph (allPaths (adjacencias graph [] x y)) y) 






