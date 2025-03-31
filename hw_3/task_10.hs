{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

type Bfn = Bool -> Bool -> Bool -> Bool

xor ::  Bool -> Bool -> Bool
xor     True    False = True
xor     False   True  = True
xor     _       _     = False

bfn_add :: Bfn -> Bfn -> Bfn
bfn_add f g x y z = xor (f x y z) (g x y z)

bfn_mul :: Bfn -> Bfn -> Bfn
bfn_mul f g x y z = (f x y z) && (g x y z)

bfn_zero :: Bfn
bfn_zero _ _ _ = False

bfn_one :: Bfn
bfn_one _ _ _ = True

bfn_not :: Bfn -> Bfn
bfn_not f x y z = not (f x y z)

bfn_print :: Bfn -> String
bfn_print f = unlines [ show (x, y, z) ++ " -> " ++ show (f x y z)
                      | x <- [False, True]
                      , y <- [False, True]
                      , z <- [False, True]
                      ]

instance Eq Bfn where
    f == g = all (\(x,y,z) -> f x y z == g x y z)
             [ (x,y,z) | x <- [False,True], y <- [False,True], z <- [False,True] ]

instance Num Bfn where
    (+) = bfn_add
    (*) = bfn_mul
    fromInteger 0 = bfn_zero
    fromInteger 1 = bfn_one
    negate = bfn_not
    signum f = if f == bfn_zero then bfn_zero else bfn_one
    abs f = f

instance Show Bfn where
  show f = "Bfn:\n" ++ bfn_print f

-- Example
exampleF :: Bfn
exampleF x y z = x && y && z

main :: IO ()
main = do
    putStrLn (show exampleF)
    if (exampleF + exampleF) == bfn_zero
        then putStrLn "Test passed: f + f == 0"
        else error "Test failed: f + f /= 0"

    if (exampleF * bfn_one) == exampleF
        then putStrLn "Test passed: f * 1 == f"
        else error "Test failed: f * 1 /= f"

    putStrLn "All tests passed."