--tabela hash
get :: [a] -> b -> [a]

put :: [a] -> b -> [a]

remove :: [a] -> b -> [a]

hasKey :: [a] -> b -> [a]

--comparando conjunto



--exercicios

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo = [(”Sergio”,”O Senhor Aneis”),(”Andre”,”Duna”),(”Fernando”,”Jonathan Strange Mr Norrell”), (”Fernando”,”A Game Thrones”)]
-- livros emprestados

livros :: BancoDados -> Pessoa -> [Livro]
	livros ls pp = [l|(p,l)<-ls, p = pp]

emprestimos :: BancoDados -> Livro ->[Pessoa]
	livros ls ll = [p|(p,l)<-ls, l = ll]

emprestado :: BancoDados -> Livro -> Bool


qtdEmprestimos :: BancoDados -> Pessoa -> Int