import System.Random


gera = do
    gen <- newStdGen
    let cs = randomRs ('0', '9') gen :: [Char]
    return (take 1 cs)
    
main :: IO ()
main = do
   a <- gera
   print a
   
