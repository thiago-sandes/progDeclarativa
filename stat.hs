
--Funcoes para calcular N caracteres

--Recebe a palavra, var contadora e retorna n. caracteres.
counter :: String -> Int -> Int

counter [] n = n

counter (x:xs) n | (x /= '\n' && x/= '\t' && x /= ' ') = counter xs (n+1)
                    | otherwise = counter xs n


-- Funcao primária

countChars :: String -> Int

countChars x = counter x 0



-- Funcoes para calcular N palavras

-- Recebe um caractere e retorna True se ele for uma letra.
isLetter :: Char -> Bool

isLetter x | ( (n >= 65 && n <= 90) || (n >= 97 && n <= 122) ) = True
           | otherwise = False
           where n = fromEnum x

-- Conta numero de palavras. Recebe a palavra, uma flag (indicadora de espaco ' ') e var. contadora.
countWords :: String -> Char -> Int -> Int

countWords [] _ n = n
countWords (x:xs) flag n | (flag == ' ' && isLetter x) = countWords xs 'E' (n+1)
                         | (x == ' ' || x == '\n' || x == '\t' || x == '.' || x == ',') = countWords xs ' ' n
                         | otherwise = countWords xs flag n
                         


-- Funcao primária

getNWords :: String -> Int

getNWords x = countWords x ' ' 0



-- Funcao para contar N linhas. Recebe a palavra, var. contadora e retorn n. linhas.

getLines :: String -> Int -> Int

getLines [] n = n

getLines (x:xs) n | (x == '\n') = getLines xs (n+1)
                    | otherwise = getLines xs n


-- Funcao primária
countLines :: String -> Int

countLines [] = 0
countLines x = getLines x 1



-- Funcao principal

stat :: String -> (Int, Int, Int)


stat x = (countChars x, getNWords x, countLines x)

