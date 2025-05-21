f :: Eq a => [a] -> [a]
f [] = []
f (x:y) = x : f (filter (/= x) y)

--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

main :: IO ()
main = do
    let _ = assert (f ([] :: [Int]) == []) ()
    let _ = assert (f [1,2,3] == [1,2,3]) ()
    let _ = assert (f [1,2,1,3,2,4,1] == [1,2,3,4]) ()
    let _ = assert (f "abracadabra" == "abrcd") ()
    let _ = assert (f [True, False, True, True, False] == [True,False]) ()

    putStrLn "All tests passed."