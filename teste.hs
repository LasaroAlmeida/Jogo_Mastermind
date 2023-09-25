-- import System.Random

-- main :: IO ()
-- main = do
--    gen <- newStdGen
--    let (numeroAleatorio, _) = randomR (0, 9) gen :: (Int, StdGen)
--    print numeroAleatorio

inverteSaida tentativa [] = []
inverteSaida tentativa (x:xs) = tentativa !! x : (inverteSaida tentativa xs)