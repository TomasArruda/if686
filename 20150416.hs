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