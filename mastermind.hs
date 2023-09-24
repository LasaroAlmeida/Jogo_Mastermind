-- Dener Luis Basilio Theodoro 201835001
-- Lásaro de Almeida Deodoro   201835004

digitos = ['1','2','3','4','5','6','7','8','9','0']


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

find1 e (x:xs) = findAux e (x:xs) 0

findAux _ _ 4 = -1
findAux e (x:xs) i = if e == x 
                        then i
                    else
                        findAux e xs (i+1)

--segredo tentativa
comparaSegredoEntrada (x:xs) (y:ys) = do 
    let resposta = ""
    let index1 = find x (y:ys)

    if index1/=(-1)
        then resposta++"X"
    else
        resposta++"-"
    return resposta;

perguntaSegredo tentativas segredo
    | tentativas <= 0 = putStrLn "Jogador 2 perdeu. O segredo não foi adivinhado."
    | otherwise = do
        tentativaSegredo <- adivinhaSegredo tentativas segredo
        if segredo /= tentativaSegredo
            then do
                putStrLn "Tentativa incorreta."
                putStrLn ""
                perguntaSegredo (tentativas - 1) segredo
            else putStrLn "Jogador 2 adivinhou o segredo corretamente!"


adivinhaSegredo valor segredo = do
    putStrLn "Chute um segredo (digitos de 0 a 9, totalizando 4 digitos)"
    putStrLn $ "Você tem " ++ show valor ++ " tentativas restantes"
    tentativaSegredo <- getLine
    return tentativaSegredo


validaTamanhoEntrada :: [Char] -> IO String
validaTamanhoEntrada str = do
        putStrLn $ str
        tentativa <- getLine
        let tamanhoEntrada = length tentativa
        if tamanhoEntrada /= 4 
            then validaTamanhoEntrada "Erro! O segredo deve ter 4 dígitos!\nDigite novamente: "
            else 
                if (validaCaracteresEntrada tentativa)
                    then return tentativa
                else 
                    validaTamanhoEntrada "Erro! O segrede deve conter apenas digitos de 0 a 9!\nDigite novamente: "

validaCaracteresEntrada [] = True
validaCaracteresEntrada (x:xs) =  if (find x digitos) /= (-1)  then validaCaracteresEntrada xs else False 

main:: IO ()
main = do
    putStrLn "====================================================================================="
    putStrLn "|--------------------------------- MASTERMIND --------------------------------------|"
    putStrLn "====================================================================================="
    let msgInicio = "Jogador 1, informe o segredo (digitos de 0 a 9, totalizando 4 digitos)"
    segredo <- (validaTamanhoEntrada msgInicio)
    putStrLn "É hora do Jogador 2 tentar adivinhar"
    perguntaSegredo 8 segredo






-- import System.Random
-- import Control.Monad (when)

-- -- Função para gerar um código secreto aleatório de quatro dígitos
-- gerarCodigoSecreto :: IO String
-- gerarCodigoSecreto = do
--   gen <- getStdGen
--   let codigo = take 4 $ randomRs ('0', '9') gen
--   return codigo

-- -- Função para avaliar um chute do jogador 2 e retornar a resposta
-- avaliarChute :: String -> String -> String
-- avaliarChute codigoSegredo chute = go codigoSegredo chute ""
--   where
--     go [] [] resposta = resposta
--     go (x:xs) (y:ys) resposta
--       | x == y = go xs ys (resposta ++ "O")
--       | y `elem` xs = go xs ys (resposta ++ "-")
--       | otherwise = go xs ys (resposta ++ "X")

-- -- Função principal do jogo
-- jogoMastermind :: IO ()
-- jogoMastermind = do
--   putStrLn "Bem-vindo ao Mastermind!"
--   putStrLn "Jogador 1, escolha um código secreto de quatro dígitos (0-9)."
--   codigoSegredo <- getLine
--   jogar codigoSegredo 1

-- -- Função para iniciar o jogo
-- jogar :: String -> Int -> IO ()
-- jogar codigoSegredo tentativa = do
--   putStrLn $ "\nTentativa " ++ show tentativa ++ ":"
--   putStrLn "Jogador 2, faça seu chute (quatro dígitos):"
--   chute <- getLine
--   let resposta = avaliarChute codigoSegredo chute
--   putStrLn $ "Resposta: " ++ resposta
--   when (resposta /= "OOOO") $ jogar codigoSegredo (tentativa + 1)

-- main :: IO ()
-- main = jogoMastermind
