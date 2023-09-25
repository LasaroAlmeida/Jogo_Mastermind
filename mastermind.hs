-- Dener Luis Basilio Theodoro 201835001
-- Lásaro de Almeida Deodoro   201835004
import System.Random

digitos = ['1','2','3','4','5','6','7','8','9','0']

randCarater = do
    gen <- newStdGen
    let cs = randomRs ('0', '9') gen :: [Char]
    return (head cs)

randPositions = auxRandPositions []

auxRandPositions acc
    | length acc == 4 = return acc
    | otherwise = do
        gen <- newStdGen
        let (n, ngen) = randomR (0, 3) gen :: (Int, StdGen)
        let novoAcc = n : acc
        if find n acc == -1
            then auxRandPositions novoAcc
        else
            auxRandPositions acc

inverteSaida tentativa [] = []
inverteSaida tentativa (x:xs) = tentativa !! x : (inverteSaida tentativa xs)



find e xs = findAux e xs 0
findAux e [] _ = -1
findAux e (x:xs) i = if e == x
                        then i
                    else findAux e xs (i+1)


geraSegredo repetidos = auxGeraSegredo [] repetidos
auxGeraSegredo segredo repetidos
    | length segredo == 4 = return segredo
    | otherwise = do
        seg <- randCarater
        if repetidos == 1
            then auxGeraSegredo (seg : segredo) repetidos
        else if find seg segredo == -1
            then auxGeraSegredo (seg : segredo) repetidos
        else auxGeraSegredo segredo repetidos


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


perguntaSegredo tentativas segredo
    | tentativas <= 0 = putStrLn $ "Você perdeu! O segredo era: " ++ segredo
    | otherwise = do
        tentativaSegredo <- adivinhaSegredo tentativas segredo
        if segredo /= tentativaSegredo
            then do
                posicoes <- randPositions
                let saida = inverteSaida (comparaSegredoEntrada tentativaSegredo segredo) posicoes
                putStrLn $ "Resposta: " ++ saida ++ (show posicoes)
                perguntaSegredo (tentativas - 1) segredo
            else putStrLn "Jogador 2 adivinhou o segredo corretamente!"


adivinhaSegredo valor segredo = do
    putStrLn "Chute um segredo (digitos de 0 a 9, totalizando 4 digitos)"
    let msg = "Você tem " ++ show valor ++ " tentativas restantes"
    tentativaSegredo <- validaTentativa msg
    return tentativaSegredo


validaTentativa :: [Char] -> IO String
validaTentativa str = do
        putStrLn $ str
        tentativa <- getLine
        let tamanhoEntrada = length tentativa
        if tamanhoEntrada /= 4 
            then validaTentativa "Erro! A tentativa deve ter 4 dígitos!\nDigite novamente: "
            else 
                if (validaCaracteresEntrada tentativa)
                    then return tentativa
                else 
                    validaTentativa "Erro! A tentativa deve conter apenas digitos de 0 a 9!\nDigite novamente: "

validaCaracteresEntrada [] = True
validaCaracteresEntrada (x:xs) =  if (find x digitos) /= (-1)  then validaCaracteresEntrada xs else False 


validaEntrada str = do
    putStrLn $ str
    entrada <- getLine
    if entrada == "s" || entrada == "S"
        then return 1
    else
        if entrada == "n" || entrada == "N"
            then return 0
        else
        validaEntrada "Digite uma opção válida!"

main:: IO ()
main = do
    putStrLn "====================================================================================="
    putStrLn "|--------------------------------- MASTERMIND --------------------------------------|"
    putStrLn "====================================================================================="
    let msgInicio = "Jogar com números repetidos? \n (S ou N)"
    repetidos <- validaEntrada msgInicio
    segredo <- geraSegredo repetidos
    putStrLn "É hora do Jogador 2 tentar adivinhar"
    perguntaSegredo 8 segredo

