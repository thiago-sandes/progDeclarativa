import Prelude as Prelude




--Retorna True se palav cabe dentro de linha.
cabe :: String -> String -> Int -> Bool

cabe palav linha n |(capacidadeLinha >= tamPalav) = True
                   | otherwise = False
                       where tamPalav = (Prelude.length palav) + 1 -- tamanho da palavra. +1 ' '
                             tamLinha = (Prelude.length linha) -- desconta '\n'
                             capacidadeLinha = (n - tamLinha)





--Percorre palavra por palavra para criar novo texto com no maximo n chars por linha.
-- Recebe lista de palavras do texto e lista para guardar saida.
processarTexto :: [String] -> [String] -> Int -> [String]

processarTexto [] lista _ = lista

processarTexto (x:xs) lista n | (lista == []) = processarTexto xs (lista ++ [x]) n
                              | (ok == True) = processarTexto xs [(y ++ " " ++ x)] n
                              | otherwise = y : processarTexto (x:xs) ys n
                              where y  = head lista
                                    ys = tail lista
                                    ok = cabe x y n -- verifica se a palavra cabe na linha

                               
                              
--Recebe lista de strings e retorna string, resultado da concatenacao dos elem. da lista, 
--separados por '\n'
finalizarTexto :: String -> [String] -> String

finalizarTexto texto [] = texto

finalizarTexto texto (x:xs) | (xs /= []) = finalizarTexto (texto ++ (x ++ "\n")) xs
                            | otherwise  = texto ++ x -- no caso da ultima linha



--Funcao principal
fill :: String -> Int -> String

fill [] _ = []

fill texto n = textoFinal
               where listaPalav = Prelude.words texto -- Recebe lista com palavras do texto
                     textoProcessado = processarTexto listaPalav [] n
                     textoFinal  = finalizarTexto [] textoProcessado
  


