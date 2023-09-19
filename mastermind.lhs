-- Dener Luis Basilio Theodoro 201835001
-- Lásaro de Almeida Deodoro   201835004

> perguntaSegredo tentativas segredo
>     | tentativas <= 0 = putStrLn "Jogador 2 perdeu. O segredo não foi adivinhado."
>     | otherwise = do
>         tentativaSegredo <- adivinhaSegredo tentativas segredo
>         if segredo /= tentativaSegredo
>             then do
>                 putStrLn "Tentativa incorreta."
>                 putStrLn ""
>                 perguntaSegredo (tentativas - 1) segredo
>             else putStrLn "Jogador 2 adivinhou o segredo corretamente!"

> adivinhaSegredo valor segredo = do
>     putStrLn "Chute um segredo (digitos de 0 a 9, totalizando 4 digitos)"
>     putStrLn $ "Você tem " ++ show valor ++ " tentativas restantes"
>     tentativaSegredo <- getLine
>     return tentativaSegredo

> main = do
>     putStrLn "Jogador 1, informe o segredo (digitos de 0 a 9, totalizando 4 digitos)"
>     segredo <- getLine
>     putStrLn "É hora do Jogador 2 tentar adivinhar"
>     perguntaSegredo 8 segredo