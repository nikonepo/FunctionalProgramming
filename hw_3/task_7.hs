-- Функции curry, uncurry
my_curry :: ((a, b) -> c) -> a -> b -> c
my_curry f a b = f (a, b)

my_uncurry :: (a -> b -> c) -> (a, b) -> c
my_uncurry = f (a, b) = f a b

-- Тестовые функции
add :: (Int, Int) -> Int
add (a, b) = a + b

add_curried :: Int -> Int -> Int
add_curried x y = x + y

--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

main :: IO ()
main = do
    let add_curried' = my_curry add
    let _ = assert (add_curried' 3 4 == 7) ()

    let add' = my_uncurry add_curried
    let _ = assert (add (3, 4) == 7) ()

    putStrLn "All tests passed."