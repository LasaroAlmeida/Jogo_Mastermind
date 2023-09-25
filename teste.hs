find e xs = findAux e xs 0
findAux e [] _ = -1
findAux e (x:xs) i = if e == x
                        then i
                    else findAux e xs (i+1)

comparaSegredoEntrada entrada segredo = auxCompara entrada segredo [] 0
auxCompara [] _ resposta _ = resposta
auxCompara (x:xs) segredo resposta i =
    let indice = (find x segredo)
    in
    if indice == i 
        then auxCompara xs segredo (resposta ++ "O") (i+1)
    else if indice == (-1) 
            then auxCompara xs segredo (resposta ++ "X") (i+1)
        else 
            auxCompara xs segredo (resposta ++ "-") (i+1)
            
-- auxCompara [] _ resposta _ = resposta
-- auxCompara (x:xs) segredo resposta i =
--     let indice = (find x segredo)
--     in
--     if indice == i 
--         then auxCompara xs segredo (resposta ++ "O") (i+1)
--     else if indice == (-1) 
--             then auxCompara xs segredo (resposta ++ "X") (i+1)
--         else 
--             auxCompara xs segredo (resposta ++ "-") (i+1)