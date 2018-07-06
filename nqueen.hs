type Solution = [Int]                                 -- Cria um tipo personalizado


-- Função que verifica se há choque em linha, coluna ou diagonal
extend :: Solution -> Int -> Maybe Solution           -- Assinatuta que recebe uma lista de inteiros e um inteiro
extend solution new = go 0 solution                   -- recebe a solução e uma possível nova entrada
  where
    n = length solution                               -- pega quantas soluções já existem
    go _ []  =  Just (solution ++ [new])              -- Caso esteja vazia, isnere 
    go i (x:xs)                                       -- caso contrário 
      | x == new                      = Nothing       -- se a solução de teste tiver como cabeça a posição proposta, faz nada 
      | abs (i - n) == abs (x - new)  = Nothing       -- se (iteração menos tamanho da solução) for igual (posição fixada menos posição proposta), faz nada
      | otherwise                     = go (i+1) xs  -- senão, verifica na próxima posição

fullExtend :: Int -> Solution -> Maybe Solution      -- Assinatura que recebe o tamanho do tabuleiro e uma solução conhecida e retorna uma possivel solução
fullExtend n partialSolution                         -- recebe o tamanho da solução desejada e a solução parcial
  | length partialSolution == n     = Just partialSolution -- se o tamanho da solução for o tamanho passado, retorna a solução
  | otherwise                       = foldr propose Nothing [0..(n-1)] -- de outra forma, concatena os elementos e aplica a função que verifica uma nova entrada
  where
    propose _ (Just xs) = Just xs                           -- Retorna a entrada
    propose x Nothing   = extend partialSolution x >>= fullExtend n -- 

nQueens :: Int -> Maybe Solution
nQueens n = fullExtend n []



ch old new (x:xs)
 | length (x:xs) == 0 = []
 | x == old = new:xs
 | otherwise = x : ch old new xs


solver  :: Int -> [Int]
solver n = check 0 n
 where
  ch old new (x:xs)
   | length (x:xs) == 0 = []
   | x == old = new:xs
   | otherwise = x : ch old new xs
  queen = [(x-x) | x <- [0 .. n ]]
  check i limit
   | i < limit = [0]
   | otherwise = [i]
      
