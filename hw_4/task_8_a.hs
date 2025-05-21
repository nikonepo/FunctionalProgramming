-- takeWhile возвращает наибольший префикс списка, в котором все элементы
-- удовлетворяют предикату

my_takeWhile :: (a -> Bool) -> [a] -> [a]
my_takeWhile a = foldr (\x xs -> if a x then x:xs else []) []

--
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
--

main :: IO ()
main = do
    assert (my_takeWhile (<3) [1,2,3,2,1] == [1,2]) "my_takeWhile (<3) [1,2,3,2,1]"
    assert (my_takeWhile (<0) [1,2,3] == [])        "my_takeWhile (<0) [1,2,3]"
    assert (my_takeWhile (<0) ([] :: [Int]) == [])  "my_takeWhile on empty list"
    assert (my_takeWhile (<11) [1..10] == [1..10])   "my_takeWhile (<11) [1..10]"
    assert (my_takeWhile (>11) [1..10] == [])        "my_takeWhile (>11) [1..10]"
    assert (my_takeWhile (<8) [1..10] == [1..7])     "my_takeWhile (<8) [1..10]"
    assert (my_takeWhile (>8) [1..10] == [])         "my_takeWhile (>8) [1..10]"

    putStrLn "All tests passed."