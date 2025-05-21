part :: Int -> Int -> [[Int]]
part m n
    | m <= 0    = []
    | m == 1    = [[n]]
    | n < 0     = []
    | otherwise = [ x : xs | x <- [0..n], xs <- part (m-1) (n-x) ]

--
assert :: Bool -> String -> IO ()
assert True  _   = return ()
assert False msg = error ("Assertion failed: " ++ msg)
--

main :: IO ()
main = do
    let p1 = part 1 5
    let _  = assert (p1 == [[5]]) ("part 1 5 should be [[5]]")

    let p0 = part 4 0
    let _  = assert (p0 == [[0,0,0,0]]) ("part 4 0 should be [[0,0,0,0]]")

    let p23 = part 2 3
    let _   = assert (p23 == [[0,3],[1,2],[2,1],[3,0]]) ("part 2 3 should be [[0,3],[1,2],[2,1],[3,0]]")

    let p32 = part 3 2
    let _   = assert (p32 == [[0,0,2],[0,1,1],[0,2,0],[1,0,1],[1,1,0],[2,0,0]]) ("part 3 2 should be [[0,0,2],[0,1,1],[0,2,0],[1,0,1],[1,1,0],[2,0,0]]")

    let p31 = part 3 1
    let _   = assert (p31 == [[0,0,1],[0,1,0],[1,0,0]]) ("part 3 1 should be [[0,0,1],[0,1,0],[1,0,0]]")

    putStrLn "All tests passed."