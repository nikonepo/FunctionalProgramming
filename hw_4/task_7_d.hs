my_any :: (a -> Bool) -> [a] -> Bool
my_any p = foldr ((||) . p) False

--
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
--

main :: IO ()
main = do
    assert (my_any (<5) [5..15]      == False)    "my_any (<5) [5..15]"
    assert (my_any (<5) [4..15]      == True)     "my_any (<5) [4..15]"
    putStrLn "All tests passed."
