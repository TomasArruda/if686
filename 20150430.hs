data Failable t = Error String | Only t deriving Show

data Fila v = NilF | Elem (Int, Int, v, Fila v) deriving Show

instance Monad Failable where
	(>>=) (Only x) f = f x
	(>>=) (Error x) _ = (Error x)
	return  x = (Only x)

criarFila :: Int -> t -> Failable (t, Fila t)
criarFila 0 _ = Error "Capacidade menor que 1"
criarFila x v = Only (v, Elem (x, 1, v, NilF))

push :: t -> Fila t -> Failable (t, Fila t)
push v (Elem (tam, pos, va, prox))
	|tam == pos = Error "Fila cheia"
	|otherwise =  Only (v, pushAux v (Elem (tam, pos, va, prox)))

pushAux :: t -> Fila t -> Fila t
pushAux v (Elem (tam, pos, va, NilF)) = (Elem (tam, pos+1, va, (Elem (tam,(pos+1),v, NilF))))
pushAux v (Elem (tam, pos, va, prox)) = (Elem (tam, pos+1, va, (pushAux v prox)))


pop :: Fila t -> Failable (t, Fila t)
pop NilF = Error "Fila vazia"
pop (Elem (tam, pos, va, prox)) =  Only (valor, retorno)
	where (retorno, valor) = popAux (Elem (tam, pos, va, prox))

popAux :: Fila t -> (Fila t, t)
popAux	(Elem (tam, pos, va, NilF)) = (NilF, va)
popAux	(Elem (tam, pos, va, prox)) = ((Elem (tam, pos, va, retorno)),valor)
	where (retorno, valor) = popAux prox

--peek :: Fila t -> Failable (t, Fila t)