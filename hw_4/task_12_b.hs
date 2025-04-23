unzipFoldr :: [(a,b)] -> ([a],[b])
unzipFoldr = foldr (\(a,b) (as,bs) -> (a:as, b:bs)) ([],[])

--
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
--

main :: IO ()
main = do
    let pairs = [(1,'a'), (2,'b'), (3,'c')]
    assert (unzipFoldr pairs == ([1,2,3], "abc")) "unzipFoldr [(1,'a'),(2,'b'),(3,'c')]"

    let pb = [(True,False),(False,True)]
    assert (unzipFoldr pb == ([True,False],[False,True])) "unzipFoldr [(True,False),(False,True)]"

    putStrLn "All tests passed."