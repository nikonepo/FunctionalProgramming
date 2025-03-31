composition :: (b -> c, a -> b) -> a -> c
composition (g, f) = g . f

composeFuncs :: [Double -> Double] -> Int -> Int
composeFuncs fs n = round $ (foldr (.) id fs) (fromIntegral n)

--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

main :: IO ()
main = do
    let test1 = composition ((+1), (*2)) 3
    let _ = assert (test1 == 7) (return ())

    let test2 = composeFuncs [(*2), (+3)] 4
    let _ = assert (test2 == 14) (return ())

    putStrLn "All tests passed."