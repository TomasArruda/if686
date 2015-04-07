--Exercícios
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle a) = (2*pi*a)
area (Rectangle a b) = a*b

data Semana = Domingo | Segunda Int [String] | Terca Int [String] | Quarta Int [String] | Quinta Int [String] | Sexta Int [String] | Sabado

fds :: Semana -> Bool
fds Domingo = True
fds Sabado = True
fds _ = False

plc :: Semana -> Bool
plc Domingo = False
plc Sabado = False
plc (Segunda a []) = False
plc (Segunda a (l:ls))
	|(l:ls) == [] = False
	|l == "PLC" = True
	|otherwise = plc (Segunda a ls)
plc (Terca a []) = False
plc (Terca a (l:ls))
	|(l:ls) == [] = False
	|l == "PLC" = True
	|otherwise = plc (Terca a ls)
plc (Quarta a []) = False
plc (Quarta a (l:ls))
	|(l:ls) == [] = False
	|l == "PLC" = True
	|otherwise = plc (Quarta a ls)
plc (Quinta a []) = False
plc (Quinta a (l:ls))
	|(l:ls) == [] = False
	|l == "PLC" = True
	|otherwise = plc (Quinta a ls)
plc (Sexta a []) = False
plc (Sexta a (l:ls))
	|(l:ls) == [] = False
	|l == "PLC" = True
	|otherwise = plc (Sexta a ls)

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

showExpr :: Expr -> String
showExpr (Lit i) = show i
showExpr (Add x y) = (showExpr x )++" + "++(showExpr y )
showExpr (Sub x y) = (showExpr x )++" - "++(showExpr y )

data List t = Nil | Cons t (List t)

toList :: List t -> [t]
toList Nil = []
toList (Cons a ls) = (a:(toList ls))

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = (Cons a (fromList as))


data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Eq, Show)

maior :: Int -> Int -> Int
maior x y
	|x > y = x
	|otherwise = y

depth :: Tree t -> Int
depth NilT = 0
depth (Node t x y) = maior (1+(depth x)) (1+(depth y))

collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node t x y) = (t:(collapse x))++(t:(collapse y))

bfs :: Tree t -> t -> Bool
bfs NilT = False
--bfs (Node t)

mapTree :: (t -> u) -> Tree t -> Tree u
mapTree func NilT = NilT
mapTree func (Node t x y) = (Node (func t) (mapTree (func) x) (mapTree (func) y))






