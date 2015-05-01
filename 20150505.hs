import Data.Char (ord)
import Data.Char (toUpper)

--qQuestão 1


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

main = do
	{
	    putStrLn "Type something, please"; 
	    word <- getLine;
	    a <- isWord word;
	    b <- change a;
	    c <- split b;
	    mapM_ putStrLn c;
	}
 