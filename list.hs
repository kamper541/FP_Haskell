-- -- list.hs

-- -- len [] = 0
-- -- len (x:xs) = len xs + 1

-- --join ((x:xs),[]) = x:xs เป็นเหมือน base case กล่าวคิือไม่ว่าจะเป็น list อะไร(x:xs)มา join กับ [] list ว่างคือจะได้ list อันนั้น(x:xs) ซึ่งคลอคลุม join([],[]) เพราะ ถ้าให้ (x:xs) = []
-- --สุดท้ายถ้า join กันมันจะได้ (x:xs) ซึ่งก็คือ [] 
-- --join ((x:xs),(y:ys)) = x : join (xs,(y:ys)) เหมือนค่อยๆจับทีละตัว มาทำการ join กัน กล้าวคือ x(ตัวแรก) :(concat) join (xs,(y:ys))(กับตัวที่เหลือ) recursive ไปเรื่อยๆ
-- --จนถึง base case คือ เจอ [] list ว่าง จนสุดท้าย list ทุกตัวมาเชื่อมกัน

-- -- join ((x:xs),[]) = x:xs
-- -- join ((x:xs),(y:ys)) = x : join (xs,(y:ys))

-- -- len [] = 0
-- -- len (x:xs) = len xs + 1

-- -- take1 0 _ = []
-- -- take1 _ [] = []
-- -- take1 n (x:xs) = x : take1 (n-1) xs

-- -- ใช้การ join เข้ามาช่วยคือ จอยตัวหลังกับตัวหน้าให้ลำดับมันสลับกัน และ basecase ก็จนกว่าใน list จะ empty โดย join ตัวหลังแบบ recursive
-- -- แล้วเอาตัวหน้าของแต่ละ list ใน recursive มาเป็นตัวหลังแทน
-- -- join ([],ys) = ys
-- -- join (x:xs,ys) = x : join (xs,ys)
-- -- rev [] = []
-- -- rev (x:xs) = join (rev xs , [x])
-- -- rev (x:xs) = rev xs ++ [x]

-- -- มันมี zip ซึ่งคิดว่า module มันครอบคลุมหมดแล้ว
-- -- zipper x y = zip x y
-- -- ถ้าจะให้เขียนทีละ step คือสร้าง basecase เมื่อการที่ zip เจอ [] ก็จะได้ [] เพราะไม่มีตัวให้จับคู่ และนำตัวแรกของทั้งสอง list มา zip ให้เป็น
-- -- คู่อันดับ หลังจากนั้นเราก็จะจอย list ของคู่อันดับนั้นไปเรื่อยๆจนถึง basecase
-- -- zipper (_,[]) = []
-- -- zipper ([],_) = []
-- -- zipper (x:xs,y:ys) = join(zip [x] [y],zipper(xs,ys))

-- -- zipper :: ([a],[b]) -> [(a,b)]
-- -- zipper ([],ys) = []
-- -- zipper (xsx,[]) = []
-- -- zipper (x:xs,y:ys) = (x,y) : zipper (xs,ys)

-- -- curry f  = \x y -> f (x,y)

-- -- g = curry f
-- -- g x y = f (x,y)
-- -- g = \x y -> f (x,y)

-- -- uncurry g = \(x,y) -> g x y
-- -- f = uncurry g
-- -- f (x,y) = g x y
-- -- f = \(x,y) -> g x y

-- -- g x = x == 0
-- -- f x = length xs

-- -- g . f = \x -> g (f x)
-- -- (g . f) = g (f x)

-- fac 0 = 1
-- fac n = if n > 0
--         then n * fac (n - 1)
--         else error "negative number"

-- ifthenelse cond iftrue iffalse =
--     if cond then iftrue else iffalse


-- -- type of filter_concat :: ([a] -> Bool) -> [[a]] -> [a]
-- -- yes use concat(prelude).
-- -- filter_concat pred l =
-- --    concat $ filter pred l
-- filter_concat _ [] = []
-- filter_concat pred (l:ls)
--   | pred l	  = l ++ filter_concat pred ls
--   | otherwise = filter_concat pred ls

-- -- type of take_while :: (a -> Bool) -> [a] -> [a]
-- take_while _ [] = []
-- take_while pred (x:xs)
--   | pred x    = x : take_while pred xs
--   | otherwise = []

-- -- delete lambda. then use (<3) . length instead

-- -- \x -> any (x<)
-- -- \x -> any ((<) x)
-- -- \x -> (any . (<)) x
-- -- \x -> any . (<)
-- contains1 = \x l -> any (x<) l

-- -- flip contains1 = contains2
-- -- contains2 = flip (any . (<))
-- contains2 = \l x -> any (x<) l

-- --uses list comprehension
-- len_comp l = sum [1 | _ <- l]

-- rewrite
-- filter (\(x,y) -> even $ x+y) l
-- concat $ map (\f -> map f [1,2,4]) (map (,) [2,3,5])

-- write reverse using foldl
reverse' x = foldl (++) [] [reverse x , []]

-- write reverse using foldr
reverse'' y = foldr (++) [] [reverse y , []]

--I think foldl is more efficiency than foldr cuz foldr do like recursive and recursive cost more time than the usual
-- one and fold do just like usaul one (loop).

-- write map using foldr
map' p []       = []
map' p xs   = foldr (\y ys -> (p y):ys) [] xs

-- wirte filter using fold
filter' p []        = []
filter' p xs        = foldr (\x xs -> if p x then x:xs else xs ) [] xs

-- write elem
-- type of elem' :: Eq a => a -> [a] -> Bool
-- i think we can use fold to write elem
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (hd:tl) =
    if x == hd then True else elem' x tl
-- map left fold use more time to compute because it need to run
-- all element in the list
-- but map right fold use more memory

-- write partition with fold (copy from the slide)
partition' p = foldr (select' p) ([], [])
select' p x (l, r)
  | p x       = (x:l, r)
  | otherwise = (l, x:r)

parition'' p = foldr (\x (l,r) ->
    if p x then (x:l, r) else (l, x:r)) ([],[])

find p (x:xs) =
    if p x
    then x
    else find p xs

find' :: (a -> Bool) -> [a] -> Maybe a
find' p []     = Nothing
find' p (x:xs) =
    if p x then Just x else find' p xs

-- define
data Month = January | February
    | March | April
    | May | June
    | July | August
    | September | October
    | November | December
  deriving (Show)

-- implement
daysInMonth :: Month -> Integer
daysInMonth m = case m of
  January -> 31
  February -> 28
  March -> 31
  April -> 30
  May -> 31
  June -> 30
  July -> 31
  August -> 31
  September -> 30
  October -> 31
  November -> 30
  December -> 31

nextMonth :: Month -> Month
nextMonth m = case m of
  January -> February
  February -> March
  March -> April
  April -> May
  May -> June
  June -> July
  July -> August
  August -> September
  September -> October
  October -> November
  November -> December
  December -> January

nextDay :: Integer -> Month ->
    (Integer, Month)
nextDay d m =
  if d == daysInMonth m
    then (1, nextMonth m)
    else (d+1, m)

data Tree a = Empty
    | Node (Tree a) a (Tree a)
  deriving (Show)

inorder Empty          = []
inorder (Node l x r)   =
    inorder l ++ [x] ++ inorder r


-- Define Map
-- type of treeMap :: (a -> b) -> Tree a -> Tree b
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Empty = Empty
treeMap f (Node l v r) =
    Node (treeMap f l) (f v) (treeMap f r)

-- Map with fold
-- type of treeFoldr :: (a -> b -> b) -> b -> Tree a -> b
treeFoldr f acc Empty = acc
treeFoldr f acc (Node l v r) =
    let acc' = treeFoldr f acc l
        acc'' = treeFoldr f acc' r
    in f v acc''

-- find height
height Empty = 0
height (Node l _ r) =
    max (height l) (height r) + 1

-- define isBST (copy from the slide)
-- type of isBST :: Ord a => Tree a -> Bool
isBST t = fst .
          foldl lt (True, Nothing) $
          inorder t
  where
    lt (False, _) _ = (False, Nothing)
    lt (True, Nothing) x = (True, Just x)
    lt (True, Just b) x = (b <= x, Just x)

-- type of if value
class IfValue a where
  boolVal :: a -> Bool

instance IfValue Int where
  boolVal 0 = False
  boolVal _ = True

instance IfValue Integer where
  boolVal 0 = False
  boolVal _ = True

instance IfValue Double where
  boolVal 0 = False
  boolVal _ = True

instance IfValue Bool where
  boolVal = id

instance IfValue Char where
  boolVal '\NUL' = False
  boolVal _ = True

-- map for maybe
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)

-- map for pair
pairMap1 :: (a -> a') ->
    (a, b) -> (a', b)
pairMap1 f (x, y) = (f x, y)

-- define maybeAp
maybeAp Nothing  _        = Nothing
maybeAp _        Nothing  = Nothing
maybeAp (Just f) (Just v) = Just (f v)

-- define init maybe
initMaybe = Just

-- define listAP
listAp fs vs = [f v | f <- fs, v <- vs]

--define initList
initList x = [x]

pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
pure id <*> v = v

-- implemetation // copy from the slide
instance Applicative ((->) r) where
	pure        = const
	(<*>) f g x = f x (g x)

(<*>) :: X (a -> b) -> X a -> X b
  where X = (->) r

(<*>) ::
  (r -> a -> b) -> (r -> a) -> (r -> b)

-- using newType instead of bool
newtype All = All { getAll :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid All where
  mempty = All True
  (All x) <> (All y) = All (x && y)

-- define Maybe bind
define function maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b

maybeBind Nothing  _ = Nothing
maybeBind (Just x) f = f x

--define Mayber list
define function listBind :: [a] -> (a -> [b]) -> [b]

listBind xs f = concat (map f xs)

-- define Either maybe
define function eitherBind :: Either r a -> (a -> Either r b) -> Either r b

eitherBind (Left e)  _ = Left e
eitherBind (right x) f = f x

-- define Arrow bind
define function arrowBind :: (r -> a) -> (a -> (r -> b)) -> (r -> b)

arrowBind h f r = f (h r) r

-- define Pairs bind
define function pairBind :: (r, a) -> (a -> (r, b)) -> (r, b)

pairBind (r1, x) f = (r1 <> r2, y)
  where (r2, y) = f x

-- Monad law Either
instance Monad (Either e) where
    Right m >>= k = k m
    Left e  >>= _ = Left e

--1st
return a >>= k
= Right a >>= k
= k a
--2nd
Left e >>= k
= Left e
Right a >>= return
= return a
= Right a
-- 3rd
Left e >>= (\x -> k x >>= h)
= Left e
= Left e >>= h
= (Left e >>= k) >>= h 
Right a >>= (\x -> k x >>= h)
= k a >>= h
= (return a >>= k) >>= h
= (Right a >>= k) >>= h 

-- Monad law  for List
instance Monad []  where
	xs >>= f = [y | x <- xs, y <- f x]

-- 1st
return a >>= k
= [a] >>= k
= [y | x <- [a], y <- k x]
= [y | y <- k a]
= k a
-- 2nd
xs >>= return
= [y | x <- xs, y <- return x]
= [y | x <- xs, y <- [x]]
= [x | x <- xs]
= xs
-- 3rd
xs >>= (\x -> k x >>= h)
= [y | x <- xs, y <- k x >>= h]
= [y | x <- xs, y <- [y' | x' <- k x, y' <- h x']]
= [y' | x <- xs, x' <- k x, y' <- h x']
= [y' | x' <- [x'' | x <- xs, x'' <- k x], y' <- h x']
= [y' | x' <- xs >>= k, y' <- h x']
= (xs >>= k) >>= h

-- Monad law for arrow
instance Monad ((->) r) where
	f >>= k = \ r -> k (f r) r
-- 1st
return a >>= k $ r
= const a >>= k $ r
= k (const a r) r
= k a $ r
-- 2nd
m >>= return $ r
= return (m r) r
= const (m r) r
= m $ r
--3rd
m >>= (\x -> k x >>= h) $ r
= (\x -> k x >>= h) (m r) r
= (k (m r) >>= h) r
= h (k (m r) r) r
= h ((m >>= k) r) r
= (m >>= k) >>= h $ r

-- Monad for pair
instance Monoid a => Monad ((,) a) where
	(u, a) >>= k = case k a of
		(v, b) -> (u <> v, b)

    