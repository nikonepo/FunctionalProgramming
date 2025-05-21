--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

nrem :: Int -> [a] -> [a]
nrem n a
  | n == 0 = a
  | otherwise = [ x | (i,x) <- zip [1..] a, i `mod` n /= 0 ]

main :: IO ()
main = do
    let _ = assert (nrem 3 [1,2,3,4,5,6,7] == [1,2,4,5,7]) ()
    let _ = assert (nrem 1 [10,20,30] == []) ()
    let _ = assert (nrem 10 [1..5] == [1,2,3,4,5]) ()
    let _ = assert (nrem 2 "abcdef" == "ace") ()

    putStrLn "All tests passed."