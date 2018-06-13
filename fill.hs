-- Função que verifica se é algum tipo de espaço no char.
isSpace :: Char -> Bool
isSpace ch = ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r' || ch == '\f' || ch == '\v'


-- Função que retorna o tamanho de uma lista qualquer
leng :: [t] -> Int
leng [] = 0
leng (x:xs) = (leng xs) + 1


-- Função que retorna uma matriz de caracteres
takeWord :: [Char] -> [[Char]]
takeWord str = case dropWhile isSpace str of
  "" -> []
  str' -> word : takeWord str''
    where (word, str'') = break isSpace str'


-- recebe uma matriz de caracteres e retorna uma lista de caracteres
matrixToList :: [[Char]] -> [Char]
matrixToList [] = []
matrixToList [a] = a
matrixToList (x:xs) = x ++ " " ++ matrixToList xs

{-
  verifica que a concatenação de suas listas de caracteres ultrapassam ou não
  um limite dado e processa de acordo, concatenando-as sem ultrapassar o limite.
-}
checkWord :: [Char] -> [[Char]] -> Int -> [Char]
checkWord str mtx size
  | ((leng new_str) - 1) < size = new_str ++ " " ++ (fill tail_mtx_list size)
  | ((leng new_str) - 1) == size = new_str ++ "\n" ++ (fill tail_mtx_list size)
  | otherwise = str ++ "\n" ++ (fill mtx_list size)
  where
    fst_mtx = head mtx
    tail_mtx = tail mtx
    new_str = str ++ " " ++ fst_mtx
    mtx_list = matrixToList mtx
    tail_mtx_list = matrixToList tail_mtx


-- função principal para receber uma string e um tamanho e imprimir as palavras
-- sem quebrar essa regra
fill :: [Char] -> Int -> [Char]
fill [] _ = []
fill _ 0 = []
fill str size
  | ((leng (matrix_complete) == 1) || (leng str <= size)) = str
  -- (primeiro ++ segundo) ++ (terceiro caso < size)
  | (((leng fst_snd)-1) <= size) = (checkWord fst_snd snd_tail size)
  | otherwise = fst_head ++ "\n" ++(fill fst_tail_list size)
  where
    matrix_complete = takeWord str
    fst_head = (head matrix_complete)
    fst_tail = (tail matrix_complete)
    fst_tail_list = (matrixToList (tail matrix_complete))
    snd_tail = (tail fst_tail)
    snd_head = (head fst_tail)
    fst_snd =  fst_head ++ " " ++ snd_head
