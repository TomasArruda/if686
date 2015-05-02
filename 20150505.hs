import Data.Char (ord)
import Data.Char (toUpper)

--Questão 1
--tabela hash
type Chave = Int
type Codigo = Int
type Tupla = (Chave,Codigo)
type HashTable = [Tupla]

search :: HashTable -> Chave -> Int -> Maybe Int
search l key n
	| n == (key `mod` (length l)) = Nothing
	| n == (length l) = (search l key 0)
	| fst (l !! n) == key = Just n
	| otherwise = (search l key (n+1))

freeSpace :: HashTable -> Chave-> Int -> Int
freeSpace l key n
	| n == (key `mod` (length l)) = (-1)
	| n == (length l) = freeSpace l key 0
	| fst (l !! n) == (-1) = n
	| otherwise = freeSpace l key (n+1)

removeElement :: HashTable -> Maybe Int -> Maybe HashTable
removeElement l Nothing = Nothing
removeElement [] (Just n) = Just []
removeElement (a:as) (Just n)
	|n == 0 = Just (((-1),(-1)):x)
	|otherwise = Just (a:x)
	where Just x = (removeElement as (Just (n-1)))

setElement :: HashTable -> Int -> Chave -> Codigo -> HashTable
setElement l (-1) key codigo = l
setElement [] n key codigo = []
setElement (a:as) n key codigo
	|n == 0 = ((key,codigo):(setElement as (n-1) key codigo))
	|otherwise = (a:(setElement as (n-1) key codigo))

get :: HashTable -> Chave -> Maybe Int
get [] key = Nothing
get hash key
	|fst (hash !! (key `mod` (length hash))) == key = Just (key `mod` (length hash))
	|otherwise = search hash key ((key `mod` (length hash))+1)

put :: HashTable -> Chave -> Codigo -> Maybe HashTable
put [] key codigo = Nothing
put hash key codigo
	|fst (hash !! (key `mod` (length hash))) == (-1) = Just (setElement hash (key `mod` (length hash)) key codigo)
	|otherwise = Just (setElement hash (freeSpace hash key ((key `mod` (length hash))+1)) key codigo)

remove :: HashTable -> Chave -> Maybe HashTable
remove [] key = Nothing
remove hash key = (removeElement hash (get hash key))

hasKey :: HashTable -> Chave -> Bool
hasKey [] key = False
hasKey hash key
	|c == (-1) = False
	|otherwise = True
	where Just c = get hash key

main1 = do
	{
		a <- put [((-1),(-1)),((-1),(-1)),((-1),(-1)),((-1),(-1)),((-1),(-1))] 7 33;
		b <- put a 4 67;
		c <- remove b 7;
		d <- put c 2 11;
		e <- put d 3 22;
		f <- remove e 4;
		d <- put c 2 33;
		e <- put d 3 43;
		f <- remove e 3;
		g <- put f 8 66;
		h <- put g 5 37;
		i <- remove h 2;
		j <- put i 1 54;
		k <- put j 45 32;
		remove k 4;
	}


--Questão 2
isWord :: String -> IO (Maybe String)
isWord str
	|(check str) = return $ Just str
	|otherwise = return $ Nothing

check :: String -> Bool
check [] = True
check (a:as)
	|(ord a < 32) || (ord a < 65 && ord a > 32) || (ord a > 90 && ord a < 97) || (ord a > 122) = False
	|otherwise = check as

change :: Maybe String -> IO (Maybe String)
change Nothing = return Nothing
change (Just []) = return (Just [])
change (Just str) = return (changeAux str)

changeAux :: String -> Maybe String
changeAux [] = Just []
changeAux (c:cs)
	|(ord c) > 90 = Just ((toUpper c):x)
	|otherwise = Just (c:x)
	where Just x = changeAux cs

split :: Maybe String -> IO [String]
split Nothing = return []
split (Just []) = return []
split (Just str) = return (splitAux str)

splitAux :: String -> [String]
splitAux [] = []
splitAux str = [(getTilSpace str)] ++ splitAux (dropAtSpace str)

getTilSpace :: String -> String
getTilSpace [] = []
getTilSpace (a:as)
	|a  == ' ' = []
	|otherwise = (a:getTilSpace as)

dropAtSpace :: String -> String
dropAtSpace [] = []
dropAtSpace (a:as)
	|a  == ' ' = as
	|otherwise = dropAtSpace as

main2 = do
	{
	    putStrLn "Type something, please"; 
	    word <- getLine;
	    a <- isWord word;
	    b <- change a;
	    c <- split b;
	    mapM_ putStrLn c;
	}
 