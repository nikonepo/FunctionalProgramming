my_map :: (a -> b) -> [a] -> [b]
my_map f = foldr ((:) . f) []

--
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
--

main :: IO ()
main = do
    assert (my_map (+1) [1,2,3] == [2,3,4])     "my_map (+1) [1,2,3]"
    putStrLn "All tests passed."
