divisors :: Integer -> [Integer]
divisors n = [i | i <- [1 .. i - 1], n `mod` i == 0]

is_friendly :: Integer -> Integer -> Bool
is_friendly n m = sum (divisors n) == m && sum (divisors m) == n

--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

main :: IO ()
main = do
    let _ = assert (is_friendly 220 284 == True) ()
    let _ = assert (not (is_friendly 10 20)) ()

    putStrLn "All tests passed."