import System.Random
import FCM

import qualified Data.Vector as V


main = do
    let 
    seed <- newStdGen 
    --print $ generateNormMatrix 7 7 seed
    print $ generateNormMatrix 7 7 seed

