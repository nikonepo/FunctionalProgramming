-- 2) Определите тип Complex, представляющий комплексные числа, и поместите его в классы Show, Eq и
--    Num математически разумным образом.

data Complex = Complex { real :: Double, imag :: Double }

instance Show Complex where
    show (Complex r i)
        | i < 0     = show r ++ " - " ++ show (abs i) ++ "i"
        | i > 0     = show r ++ " + " ++ show i ++ "i"
        | otherwise = show r

instance Eq Complex where
    (Complex r1 i1) == (Complex r2 i2) = r1 == r2 && i1 == i2

instance Num Complex where
    (Complex r1 i1) + (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)
    (Complex r1 i1) * (Complex r2 i2) = Complex (r1 * r2 - i1 * i2) (r1 * i2 + i1 * r2)
    abs (Complex r i) = Complex (sqrt (r^2 + i^2)) 0
    signum (Complex r i) = Complex (r / mag) (i / mag)
        where mag = sqrt (r^2 + i^2)
    fromInteger n = Complex (fromInteger n) 0
    negate (Complex r i) = Complex (-r) (-i)

-- Tests
main :: IO ()
main = do
    -- 3 + 4i
    let z1 = Complex 3 4

    -- 1 - 2i
    let z2 = Complex 1 (-2)
    print z1
    print z2

    print (z1 == z2)

    let sumZ = z1 + z2
    print sumZ

    let productZ = z1 * z2
    print productZ