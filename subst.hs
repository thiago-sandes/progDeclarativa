subst :: String -> String -> String -> String
subst [] termoProcurado novoTermo = []
subst texto termoProcurado novoTermo = loop texto
  where
    loop [] = []
    loop texto =
      let (prefixo, resto) = splitAt n texto
      in
        if termoProcurado == prefixo                -- encontrou primeira ocorrência?
        then 
                novoTermo ++ resto            -- sim: faça substituição
        else head texto : loop (tail texto) -- não: continue procurando
    n = length termoProcurado
