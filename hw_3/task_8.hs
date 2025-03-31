-- Пусть A, B, C, D это Int

g :: Int -> Int -> Int
g x y = x + y

h :: Int -> Int
h x = x + 1

f :: Int -> Int -> Int
f = curry ( h . uncurry g)

--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

main :: IO ()
main = do
    let a = 3
    let b = 4

    let _ = assert (f a b == h(g a b)) ()
    putStrLn $ "f " ++ show a ++ " " ++ show b ++ " = " ++ show (f a b)
    putStrLn "All tests passed."