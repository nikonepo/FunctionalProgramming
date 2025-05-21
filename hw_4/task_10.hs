prefixes :: [a] -> [[a]]
prefixes []     = []
prefixes (x:xs) = let ps = prefixes xs in [x] : map (x:) ps
--
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
--

main :: IO ()
main = do
    assert (prefixes ([] :: [Int]) == [])       "empty"
    assert (prefixes "a" == ["a"])              "a"
    assert (prefixes "ab" == ["a", "ab"])       "ab"

    putStrLn "All tests passed."