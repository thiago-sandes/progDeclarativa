newLine = '\n'
-- função que retona um array de tamanho N preenchido com zeros
nil_arr :: Int -> [Int]
nil_arr 0 = []
nil_arr size = [0] ++ (nil_arr (size-1))


-- concat_with :: [[Char]] -> [Char] -> Int -> [Char]
-- concat_with [[z]] _ _ = [z]
-- concat_with _ _ 0 = []
-- concat_with (head_org:tail_org) (head_aux:tail_aux) size
--   | ((leng head_org) + (leng head_tail_org)) <= size
--       = -- head_org ++
--       head_tail_org -- ++
--       -- (concat_with tail_tail_org tail_tail_aux size)
--   | otherwise = []
--   where
--     head_tail_org = tail tail_org
--     tail_tail_org = tail tail_org
--     tail_tail_aux = tail (tail tail_aux)
--     --tail_tail_



mostra str = putStrLn (show str)










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
  | ((leng new_str) - 1) < size = new_str ++ " " ++ (just tail_mtx_list size)
  | ((leng new_str) - 1) == size = new_str ++ [newLine] ++ (just tail_mtx_list size)
  | otherwise = str ++ [newLine] ++ (just mtx_list size)
  where
    fst_mtx = head mtx
    tail_mtx = tail mtx
    new_str = str ++ " " ++ fst_mtx
    mtx_list = matrixToList mtx
    tail_mtx_list = matrixToList tail_mtx

{- função que incrementa os espaços -}
--
-- checkSpace :: [[Char]] -> [Int] -> [Char]
-- checkSpace (x:xs) spaces
--   |


-- função principal para receber uma string e um tamanho e imprimir as palavras
-- sem quebrar essa regra
just :: [Char] -> Int -> [Char]
just [] _ = []
just _ 0 = []
just str size
  | ((leng (matrix_complete) == 1) || (leng str <= size)) = str
  -- (primeiro ++ segundo) ++ (terceiro caso < size)
  | (((leng fst_snd)-1) <= size) = (checkWord fst_snd snd_tail size)
  | otherwise = fst_head ++ [newLine] ++(just fst_tail_list size)
  where
    matrix_complete = takeWord str
    fst_head = (head matrix_complete)
    fst_tail = (tail matrix_complete)
    fst_tail_list = (matrixToList (tail matrix_complete))
    snd_tail = (tail fst_tail)
    snd_head = (head fst_tail)
    fst_snd =  fst_head ++ " " ++ snd_head
