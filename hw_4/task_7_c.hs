my_all :: (a -> Bool) -> [a] -> Bool
my_all p = foldr ((&&) . p) True

--
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
--

main :: IO ()
main = do
    assert (my_all (>0) [1,2,3]      == True)      "my_all (>0) [1,2,3]"
    assert (my_all (<3) [1,2,3]      == False)     "my_all (<3) [1,2,3]"
    putStrLn "All tests passed."
