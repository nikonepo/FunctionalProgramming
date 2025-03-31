xor ::  Bool -> Bool -> Bool
xor     True    False = True
xor     False   True  = True
xor     _       _     = False

maj3 :: Bool -> Bool -> Bool -> Bool
maj3    True    True    _     = True
maj3    True    _       True  = True
maj3    _       True    True  = True
maj3    _       _       _     = False

--
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
--

main :: IO ()
main = do
    let _ = assert (xor True False == True) ()
    let _ = assert (xor False True == True) ()
    let _ = assert (xor True True == False) ()
    let _ = assert (xor False False == False) ()

    let _ = assert (maj3 True True True == True) ()
    let _ = assert (maj3 True True False == True) ()
    let _ = assert (maj3 True False True == True) ()
    let _ = assert (maj3 False True True == True) ()
    let _ = assert (maj3 True False False == False) ()
    let _ = assert (maj3 False True False == False) ()
    let _ = assert (maj3 False False True == False) ()
    let _ = assert (maj3 False False False == False) ()

    putStrLn "All tests passed."