-- dropWhile удаляет из списка подряд идущие элементы, которые удовлетворяют предикату.
-- Как только элемент не удовлетворяет предикату, dropWhile возвращает остаток списка.

my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile _ [] = []
my_dropWhile p (x:xs)
    | p x = my_dropWhile p xs
    | otherwise = x:xs

--
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
--

main :: IO ()
main = do
    assert (my_dropWhile (<3) [1,2,3,2,1] == [3,2,1])       "my_dropWhile (<3) [1,2,3,2,1]"
    assert (my_dropWhile (<0) [1,2,3] == [1,2,3])           "my_dropWhile (<0) [1,2,3]"
    assert (my_dropWhile (<0) ([] :: [Int]) == [])          "my_dropWhile on empty list"
    assert (my_dropWhile (<11) [1..10] == [])               "my_dropWhile (<11) [1..10]"
    assert (my_dropWhile (>11) [1..10] == [1..10])          "my_dropWhile (>11) [1..10]"
    assert (my_dropWhile (<8) [1..10] == [8..10])           "my_dropWhile (<8) [1..10]"
    assert (my_dropWhile (>8) [1..10] == [1..10])           "my_dropWhile (>8) [1..10]"

    putStrLn "All tests passed."