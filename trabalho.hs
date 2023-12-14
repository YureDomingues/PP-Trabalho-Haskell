module Main where

{- LUIZ EDUARDO E YURE SOUZA -}

{- FUNÇÃO INTERVALO -}
intervalo caractereInicio inicio fim caractereFim
    | inicio > fim = []
    | inicio <= fim = case (caractereInicio, caractereFim) of
        ('[', ']') -> [inicio..fim]
        ('[', ')') -> [inicio..fim-1]
        ('(', ']') -> [inicio+1..fim]
        ('(', ')') -> [inicio+1..fim-1]

{- FUNÇÃO CONTÉM -}
contem x [] = False
contem x lista 
    | x == (head lista) = True
    | otherwise = contem x (tail lista)


{- FUNÇÃO INTERSECÇÃO -}
intersecção l [] = False
intersecção [] l = False
intersecção (cab:corpo) lista
  | contem cab lista = True 
  | otherwise = intersecção corpo lista

{- FUNÇÃO INTERSECÇÃO (Numeros) -}
inser l [] = []
inser [] l = []
inser (cab:corpo) lista
  | contem cab lista = cab : inser corpo lista 
  | otherwise = inser corpo lista


{- FUNÇÃO MÉDIA -}
tamanho [] = 0
tamanho (cab:corpo) = 1 + tamanho corpo

soma [] = 0
soma (cab:corpo) = cab + soma corpo

media [] = 0
media lista = soma lista / tamanho lista

{- FUNÇÃO PRODUTO LIMITES -}

produto lista1 lista2 = 
    intervalo '[' (minimum [inf1*inf2, inf1*sup2, sup1*sup2]) (maximum [inf1*inf2, inf1*sup2, sup1*sup2]) ']'
  where
    inf1 = minimum lista1
    sup1 = maximum lista1
    inf2 = minimum lista2
    sup2 = maximum lista2


{- FUNÇÃO UNIÃO -}
juntar l [] = l
juntar [] l = l
juntar (cab:corpo) (cab2:corpo2)
  | cab == cab2 = cab: juntar corpo corpo2
  | cab < cab2 = cab: juntar corpo (cab2:corpo2)
  | otherwise = cab2: juntar (cab:corpo) corpo2



main = do
-- Criando os intervalos
  let inter1 = intervalo '(' (-1) 3 ']'
  let inter2 = intervalo '(' 7 10 ')'
  print inter1
  print inter2

-- Verificar se um elemento X existe na lista
  print (contem 1 inter1)
  print (contem 1 inter2)

-- A intersecção das 2 listas
  print (intersecção inter1 inter2)
  print (inser inter1 inter2)

-- Média das listas
  print (media inter1)
  print (media inter2)

-- Produto dos limites
  print (produto inter1 inter2)

-- União das listas
  print (juntar inter1 inter2)
  