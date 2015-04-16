--trabalho
quicksort :: (Ord t, Num t) => [t] -> [t]
quicksort [] = []
quicksort (p:ls) = (quicksort [a|a<-ls,a<p])++[p]++(quicksort [b|b<-ls,b>=p]) 

listPartitioner :: (Ord a, Num a) => [a] -> ([a] ->[[a]])
listPartitioner l = (\x->partitioner (quicksort l) x) 

partitioner :: (Ord a, Num a) => [a] -> [a] -> [[a]] 
partitioner [] l = [l]
partitioner _ [] = [[]]
partitioner (n:[]) l = [[r|r<-l,r<n]]++[[t|t<-l,t>=n]]
partitioner (n:ns) l = ([r|r<-l,r<n]:(partitioner ns [t|t<-l,t>=n]))

--exercicios

--inter e diff
inter ::(Eq t) => [t] -> [t] -> [t]
inter l1 l2 = filter (\x -> (elem x l2)) l1

diff ::(Eq t) => [t] -> [t] -> [t]
diff l1 l2 = filter (\x -> (not (elem x l2))) l1

--mapfilter
mapfilter :: (a -> Bool) -> [[a]] -> [[a]]
mapfilter func l =  meuMap (meuFilter func) l

meuMap :: (t->a) -> [t] -> [a]
meuMap func l = [func e|e<-l]

meuFilter :: (t->Bool) -> [t] -> [t]
meuFilter func l = [e|e<-l, func e]

--(\... -> ...)
f x y = x 

m = \x y -> f y x

--funçoões com lambda
primeiro = \l-> [x|(x,y)<-l]
segundo = \ll n -> [l|l<-ll,(length l)>n]
terceiro = \ll-> removerRep (quicksort (juntar ll))

juntar = \ll -> foldr (++) [] ll

removerRep :: [Int] -> [Int]
removerRep [] = []
removerRep ls = [(ls!!0)]++removerRep (drop rep ls)
		where rep = contador ls (ls !! 0)

contador :: [Int] -> Int -> Int
contador [] _ = 0
contador (a:as) v
	|a == v = 1+(contador as v)
	|otherwise = 0+(contador as v) 


