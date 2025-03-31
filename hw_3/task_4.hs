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
    let _ = assert (f 0 == 1) ()
    let _ = assert (f 1 == 1) ()
    let _ = assert (f 2 == 3) ()
    let _ = assert (f 3 == 14) ()

    putStrLn "All tests passed."