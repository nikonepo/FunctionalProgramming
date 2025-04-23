import Data.List (inits, tails, sort)

segs :: [a] -> [[a]]
segs = concatMap (tails . inits) . tails

--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

main :: IO ()
main = do
    let s       = "hello"
        gotS    = sort (segs s)
        wantS   = sort
        [ "h","e","l","l","o"
        , "he","el","ll","lo"
        , "hel","ell","llo"
        , "hell","ello"
        , "hello"
        ]
    assert (gotS == wantS)
        ("segs \"hello\" =\n  " ++ show gotS ++ "\n but expected\n  " ++ show wantS)

    putStrLn "Test segs \"hello\" passed."

    let xs      = [1,2,3] :: [Int]
        gotX    = sort (segs xs)
        wantX   = sort [[1],[1,2],[1,2,3],[2],[2,3],[3]]
    assert (gotX == wantX)
        ("segs [1,2,3] = " ++ show gotX ++ " but expected " ++ show wantX)

    putStrLn "Test segs [1,2,3] passed."

    putStrLn "All tests passed."