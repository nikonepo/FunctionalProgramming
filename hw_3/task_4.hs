fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = f(x-1) + f(x-2)

f :: Integer -> Integer
f n = sum [ i ^ (fib n) | i <- [0..n] ]

--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

main :: IO ()
main = do
    assert (f 0 == 1) (k+273.15)
    assert (f 1 == 1) (k+273.15)
    assert (f 2 == 3) (k+273.15)
    assert (f 3 == 14) (k+273.15)

    putStrLn "All tests passed."