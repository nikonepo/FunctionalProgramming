f :: Eq a => [a] -> [a]
f [] = []
f (x:y) x : f (filter (/= x) y)

--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

main :: IO ()
main = do
    let _ = assert (remDups ([] :: [Int]) == []) ()
    let _ = assert (remDups [1,2,3] == [1,2,3]) ()
    let _ = assert (remDups [1,2,1,3,2,4,1] == [1,2,3,4]) ()
    let _ = assert (remDups "abracadabra" == "abrcd") ()
    let _ = assert (remDups [True, False, True, True, False] == [True,False]) ()

    putStrLn "All tests passed."