--trabalhos

--tabela hash
type Chave = Int
type Codigo = Int
--type Tupla = (Chave,Codigo)
type HashTable = [Int]

search :: HashTable -> Chave -> Int -> Int
search l key n
	| n == (key `mod` (length l)) = (-1)
	| n == (length l) = search l key 0
	| l !! n == key = n
	| otherwise = search l key (n+1)

freeSpace :: HashTable -> Chave-> Int -> Int
freeSpace l key n
	| n == (key `mod` (length l)) = (-1)
	| n == (length l) = freeSpace l key 0
	| l !! n == (-1) = n
	| otherwise = freeSpace l key (n+1)

removeElement :: [Int] -> Int -> [Int]
removeElement l (-1) = l
removeElement [] n = []
removeElement (a:as) n
	|n == 0 = ((-1):(removeElement as (n-1)))
	|otherwise = (a:(removeElement as (n-1)))

setElement :: [Int] -> Int -> Chave -> [Int]
setElement l (-1) key = l
setElement [] n key = []
setElement (a:as) n key
	|n == 0 = (key:(setElement as (n-1) key))
	|otherwise = (a:(setElement as (n-1) key))

get :: HashTable -> Chave -> Int
get [] key = (-1)
get hash key
	|hash !! (key `mod` (length hash)) == key = (key `mod` (length hash))
	|otherwise = search hash key ((key `mod` (length hash))+1)

put :: HashTable -> Chave -> HashTable
put [] key = []
put hash key
	|hash !! (key `mod` (length hash)) == (-1) = setElement hash (key `mod` (length hash)) key
	|otherwise = setElement hash (freeSpace hash key ((key `mod` (length hash))+1)) key

remove :: HashTable -> Chave -> HashTable
remove [] key = []
remove hash key = removeElement hash (get hash key)

hasKey :: HashTable -> Chave -> Bool
hasKey [] key = False
hasKey hash key
	|get hash key == (-1) = False
	|otherwise = True

--comparando conjunto

quicksort :: (Ord t) => [t] -> [t]
quicksort [] = []
quicksort (p:ls) = quicksort([e1|e1<-ls, e1<=p])++[p]++quicksort([e2|e2<-ls, e2>p])

elimRep :: (Ord t) => [t] -> [t]
elimRep [] = []
elimRep [a] = [a]
elimRep (a:a2:as)
	|a == a2 = (elimRep (a2:as))
	|otherwise = (a:elimRep (a2:as))

contem :: (Ord t) => [t] -> [t] -> Bool
contem [] [] = True
contem [] l = True
contem l [] = False
contem (a:as) (b:bs)
	|a /= b = contem (a:as) bs
	|otherwise = contem as bs

igual :: (Ord t) => [t] -> [t] -> Bool
igual [] [] = True
igual [] l = False
igual l [] = False
igual (a:as) (b:bs)
	|a /= b = False
	|otherwise = igual as bs

existe ::(Ord t) => t -> [t] -> Bool
existe n [] = False
existe n (a:as)
	|n == a = True
	|otherwise = existe n as
	
intercesiona :: (Ord t) => [t] -> [t] -> Bool
intercesiona [] [] = False
intercesiona [] l = False
intercesiona l [] = False
intercesiona (a:as) (b:bs)
	|a == b = True
	|existe a bs == True = True
	|existe b as == True = True 
	|otherwise = False
	
comparaConjuntos :: (Ord t) => [t] -> [t] -> String
comparaConjuntos a b
	|contem (elimRep (quicksort a)) (elimRep (quicksort b)) == True = "B contem A"
	|contem (elimRep (quicksort b)) (elimRep (quicksort a)) == True = "A contem B"
	|igual (elimRep (quicksort a)) (elimRep (quicksort b)) == True = "A igual B"
	|intercesiona (elimRep (quicksort a)) (elimRep (quicksort b)) == True = "A intercesiona B"
	|otherwise = "Conjuntos disjuntos" 


--exercicios

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

--baseExemplo :: BancoDados
--baseExemplo = [(”Sergio”,”O Senhor Aneis”),(”Andre”,”Duna”),(”Fernando”,”Jonathan Strange Mr Norrell”), (”Fernando”,”A Game Thrones”)]
-- livros emprestados

livros :: BancoDados -> Pessoa -> [Livro]
livros ls pp = [l|(p,l)<-ls, p == pp]

emprestimos :: BancoDados -> Livro ->[Pessoa]
emprestimos ls ll = [p|(p,l)<-ls, l == ll]

emprestado :: BancoDados -> Livro -> Bool
emprestado [] ll = False
emprestado ((p,l):ls) ll
	| l == ll = True
	|otherwise = emprestado ls ll

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] pp = 0
qtdEmprestimos ((p,l):ls) pp
	| p == pp = 1 + qtdEmprestimos ls pp
	| otherwise = 0 + qtdEmprestimos ls pp





