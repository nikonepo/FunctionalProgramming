f :: Ordering -> Either () Bool
f EQ = Left()
f LT = Right False
f GT = Right True

g :: Either () Bool -> Ordering
g (Left ())     = EQ
g (Right False) = LT
g (Right True)  = GT

--

f (g Left())        = f EQ = Left()
f (g Right False)   = f LT = Right False
f (g Right True)    = f GT = Right True

g (f EQ) = g (Left())       = EQ
g (f LT) = g (Right False)  = LT
g (f GT) = g (Right True)   = GT

