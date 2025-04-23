-- unzip функция - обратная к zip. Возвращает 2 списка, 1 - из первых компонент пар, 2 - из вторых.

unzipMap :: [(a,b)] -> ([a],[b])
unzipMap xs = (map fst xs, map snd xs)

--
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
--

main :: IO ()
main = do
    let pairs = [(1,'a'), (2,'b'), (3,'c')]
    assert (unzipMap pairs == ([1,2,3], "abc")) "unzipMap [(1,'a'),(2,'b'),(3,'c')]"

    let pb = [(True,False),(False,True)]
    assert (unzipMap pb == ([True,False],[False,True])) "unzipMap [(True,False),(False,True)]"

    putStrLn "All tests passed."