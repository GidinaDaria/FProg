module Main
where
       
import qualified Data.Vector as V
import Test.HUnit
import FCM as F

main :: IO()
main = do
    let 
        checkHamming = TestCase (
                                     assertEqual "Check hamming distance beetween [1,1,1,1] [0,1,0,0])" 3.0 
                                    ( 
                                        hammingDistance (V.fromList [1.0,1.0,1.0,1.0]) (V.fromList [0.0,1.0,0.0,0.0])
                                    )
                                )
        checkEuclide = TestCase (
                                     assertEqual "Check euclide distance beetween [1,1,1,1] [0,1,0,0])" 1.7320508075688772 
                                    ( 
                                        euclideDistance (V.fromList [1.0,1.0,1.0,1.0]) (V.fromList [0.0,1.0,0.0,0.0])
                                    )
                                )

        checkMatrixTest = TestCase (
                                     assertEqual "Check matrix" 7.0 
                                    ( 
                                        checkMatrix (F.toVectorMatrix [[1.0,1.0],[2.0,2.0]]) (F.toVectorMatrix [[3.0,2.0],[1.0,9.0]])
                                    )
                                )
 
    counts <- runTestTT (TestList [checkHamming,checkEuclide,checkMatrixTest] )   
    return ()