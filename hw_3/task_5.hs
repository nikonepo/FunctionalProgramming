pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples = [ (x, y, z)
                        | z <- [1..]
                        , x <- [1..z]
                        , y <- [x..z]
                        , x^2 + y^2 == z^2
                     ]

--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

main :: IO ()
main = do
    -- В CI можно посмотреть вывод троек
    print (take 10 pythagoreanTriples)