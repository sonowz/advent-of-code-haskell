import Debug.Trace

diff (x1:x2:xs) = (x2 - x1) : diff (x2:xs)
diff _ = []

($$) :: a -> (a -> b) -> b
($$) = flip ($)

updateB :: Vector (Vector a) -> (Int, Int) -> a -> Vector (Vector a)
updateB b (x, y) v = Vec.accum ($$) b [(y, \l -> Vec.accum ($$) l [(x, const v)])]

mapMaybeB :: (a -> Maybe b) -> Vector (Vector a) -> [b]
mapMaybeB f b = (catMaybes . Vec.toList . Vec.concatMap (Vec.map f)) b

imapB :: ((Int, Int) -> a -> b) -> Vector (Vector a) -> Vector (Vector b)
imapB f b = Vec.imap (\y xs -> Vec.imap (\x o -> f (x, y) o) xs) b

imapMaybeB :: ((Int, Int) -> a -> Maybe b) -> Vector (Vector a) -> [b]
imapMaybeB f b = (catMaybes . Vec.toList . iconcatMap (\y l -> Vec.imap (\x v -> f (x, y) v) l))  b where
    iconcatMap f = Vec.concat . Vec.toList . Vec.imap f
