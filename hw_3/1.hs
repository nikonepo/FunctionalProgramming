composition :: (b -> c, a -> b) -> a -> c
composition (g, f) = g . f

composeFuncs :: [Double -> Double] -> Int -> Int
composeFuncs fs n = round $ (foldr (.) id fs) (fromIntegral n)

main :: IO ()
main = do
    let test1 = composition ((+1), (*2)) 3
    putStrLn $ "Test 1: composition ((+1), (*2)) 3 = " ++ show test1
    if test1 == 7
      then putStrLn "Test 1 passed."
      else putStrLn "Test 1 failed."

    let test2 = composeFuncs [(*2), (+3)] 4
    putStrLn $ "Test 2: composeFuncs [(*2), (+3)] 4 = " ++ show test2
    if test2 == 14
      then putStrLn "Test 2 passed."
      else putStrLn "Test 2 failed."