f :: (Maybe A, Maybe B) -> Maybe (Either (A,B) (Either A B))
f (Nothing, Nothing)    = Nothing
f (Just a, Just b)      = Just (Left (a,b))
f (Just a, Nothing)     = Just (Right (Left a))
f (Nothing, Just b)     = Just (Right (Right b))

g :: Maybe (Either (A,B) (Either A B)) -> (Maybe A, Maybe B)
g Nothing                   = (Nothing, Nothing)
g (Just (Left (a,b))        = (Just a, Just b)
g (Just (Right (Left a)))   = (Just a, Nothing)
g (Just (Right (Right b)))  = (Nothing, Just b)

--

g (f (Nothing, Nothing)) = g Nothing = (Nothing, Nothing)
g (f (Just a, Just b)) = g (Just (Left (a,b))) = (Just a, Just b)
g (f (Just a, Nothing)) = g (Just (Ring (Left a))) = (Just a, Nothing)
g (f (Nothing, Just b)) = g (Just (Right (Right b))) = (Nothing, Just b)

f (g Nothing) = f (Nothing, Nothing) = Nothing
f (g Just (Left (a,b))) = f (Just a, Just b) = Just (Left (a,b))
f (g Just (Right (Left a))) = f (Just a, Nothing) = Just (Right (Left a))
f (g Just (Right (Right a))) = f (Nothing, Just b) = Just (Right (Right b))