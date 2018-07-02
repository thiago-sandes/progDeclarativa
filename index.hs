import Data.Char as Char hiding (isLetter) --USADA APENAS PARA A FUNCAO 'toLower', como permitido pelo senhor.
import Prelude as Prelude


--Recebe string e lista. Remove da lista a primeira ocorrencia da string.
removerItem :: String -> [String] -> [String]

removerItem _ [] = []

removerItem x (y:ys) | (x == y) = ys
                     | otherwise = y : removerItem x ys



--Recebe elemento inicial da lista da qual se quer conhecer o menor elemento. Retorna menor elemento.
getMenorItem :: String -> [String] -> String

getMenorItem menor [] = menor

getMenorItem menor (x:xs) | (x < menor) = getMenorItem x xs
                          | otherwise = getMenorItem menor xs



--Recebe lista desordenada e lista vazia. Retorna lista ordenada.
ordenar :: [String] -> [String] -> [String]

ordenar [] lista = lista

ordenar (x:xs) listOrd = ordenar listaDesord (listOrd ++ [menor])
                         where menor = getMenorItem x (x:xs)
                               listaDesord = removerItem menor (x:xs)




--Converte string para letras minusculas
textToLower :: String -> String -> String

textToLower [] newStr = newStr
textToLower (x:xs) newStr = textToLower xs (newStr ++ [Char.toLower x])


-- Recebe um caractere e retorna True se ele for uma letra.
isLetter :: Char -> Bool

isLetter x | ( (n >= 65 && n <= 90) || (n >= 97 && n <= 122) || (n >= 128 && n <= 165) || (n >= 48 && n <= 57) ) = True
           | otherwise = False
           where n = fromEnum x


--Returna True se string estiver na lista. 
estaNaLista :: String -> [String] -> Bool

estaNaLista _ [] = False

estaNaLista x (y:ys) | (x == y) = True
                     | otherwise = estaNaLista x ys



--Retorna string contendo apenas o caracteres da string recebida que sejam letra, '\n', '\t' ou ' '
removerCharsEspeciais :: String -> String -> String

removerCharsEspeciais [] str = str

removerCharsEspeciais (x:xs) str 
  | (isLetter x == True || x == ' ' || x == '\t' || x == '\n') = removerCharsEspeciais xs (str ++ [x])
  | otherwise = removerCharsEspeciais xs (str ++ " ") --para considerar pontuacao como espaco



--Retorna True se str1 esta contida em str2; False, caso contrario.
visitada :: String -> String -> Bool

visitada x [] = False
visitada str1 str2 = estaNaLista str1 y
                      where y = Prelude.words str2 --Transforma em lista de palavras



-- Recebe, nesta ordem: texto, var. auxiliar (inicialmente []) e a lista (inic.[]) p/ guardar as palav.
-- Recebe texto e retorna dividido em linhas. O separador eh o '\n'
dividirString :: String -> String -> [String] -> [String]


dividirString [] aux lista = lista ++ [aux]
dividirString (x:xs) aux lista | (x == '\n') = dividirString xs [] (lista ++ [aux])
                       | otherwise = dividirString xs (aux ++ [x]) lista



--Recebe texto e string. Retorna lista com linhas de ocorrencia dessa string no texto.
getOcurrencies :: [String] -> String -> [Int] -> Int -> [Int]

getOcurrencies [] _ listOcorrenc _ = listOcorrenc
getOcurrencies (x:xs) string listOcorrenc linha
      | (estaNaLista string linhaTexto == True) = getOcurrencies xs string (listOcorrenc ++ [linha]) (linha+1)
      | otherwise = getOcurrencies xs string listOcorrenc (linha+1)
      where linhaTexto = Prelude.words x

            


-- Retorna lista com tuplas. Recebe texto em forma de lista. Recebe lista de palavras a serem consultadas.
getIndexes :: [String] -> [String] -> [(String, [Int])] -> String -> [(String, [Int])]

getIndexes _ [] lista _ = lista
getIndexes listLinhas (x:xs) lista listConsultas | (visitada x listConsultas == False) = getIndexes listLinhas xs (lista ++ [(x,getOcurrencies listLinhas x [] 0)]) (listConsultas ++ " "++ x)
                                                 | otherwise = getIndexes listLinhas xs lista listConsultas




--FUNCAO PRINCIPAL
index :: String -> [(String, [Int])]

index x = getIndexes listLinhas listPalavrasOrdenadas [] []
          where texto = textToLower x [] --Passa as letras do texto para minusculas
                str = removerCharsEspeciais texto [] --Remove o que nao eh palavra
                listPalavras = Prelude.words str -- Obtem lista com palavras do texto
                listPalavrasOrdenadas = ordenar listPalavras [] -- Obtem lista ordenada de palavras
                listLinhas = dividirString str [] [] -- Transforma texto em lista, onde cada elemento
                                                     -- da lista eh uma linha do texto.



