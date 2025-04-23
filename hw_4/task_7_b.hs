my_filter :: (a -> Bool) -> [a] -> [a]
my_filter p = foldr (\x -> if p x then (x:) else id) []

--
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
--

main :: IO ()
main = do
    assert (my_filter (>'c') "abcdef" == "def")    "my_filter (> 'c')"
    putStrLn "All tests passed."
