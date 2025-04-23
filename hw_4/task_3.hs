import Data.List (inits, tails, sort)

segs :: [a] -> [[a]]
segs = concatMap (tail . inits) . tails

--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

main :: IO ()
main = do
    let s123 = segs [1,2,3]
    let _ = assert (s123 == [[1],[1,2],[1,2,3],[2],[2,3],[3]]) ()

    let segH = segs "hello"
        expectedOrder = [ "h","he","hel","hell","hello", "e","el","ell","ello", "l","ll","llo", "l","lo", "o"]
    let _ = assert (segH == expectedOrder) ()

    putStrLn "All tests passed."