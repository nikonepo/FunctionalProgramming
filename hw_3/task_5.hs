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
    print (take 10 pythagoreanTriples)