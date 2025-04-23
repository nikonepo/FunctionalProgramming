rotts :: [a] -> [[a]]
rotts a = take (length a) (iterate (\(x:xs) -> xs ++ [x]) a)

--
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
--

main :: IO ()
main = do
    assert (rotts ([] :: [Int]) == [])  "rotts []"
    assert (rotts [42] == [[42]])       "rotts [42]"
    assert (rotts [1,2,3] == [[1,2,3],[2,3,1],[3,1,2]]) "rotts [1,2,3]"
    assert (rotts "abc" == ["abc","bca","cab"]) "rotts \"abc\""

    putStrLn "All tests passed."