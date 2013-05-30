
module Map where

data Map = Tip | Bin Size String Integer Map Map

type Size = Integer

instance Show Map where
    show m = show (toList m)

lookup :: String -> Map -> Maybe Integer
lookup k t
  = case t of
      Tip -> Nothing
      Bin _ kx x l r ->
        case compare k kx of
          LT -> Map.lookup k l
          GT -> Map.lookup k r
          EQ -> Just x
      
insert :: String -> Integer -> Map -> Map
insert kx x t =
  case t of
    Tip -> singleton kx x
    Bin sz ky y l r ->
        case compare kx ky of
           LT -> balance ky y (insert kx x l) r
           GT -> balance ky y l (insert kx x r)
           EQ -> Bin sz kx x l r

delta :: Integer
delta = 5

ratio :: Integer
ratio = 2

balance :: String -> Integer -> Map -> Map -> Map 
balance k x l r =
  let sizeL = size l
      sizeR = size r
      sizeX = sizeL + sizeR + 1
  in if (sizeL + sizeR <= 1)
        then Bin sizeX k x l r
        else if (sizeR >= delta*sizeL)
                then rotateL k x l r
                else if (sizeL >= delta*sizeR)
                    then rotateR k x l r
                    else Bin sizeX k x l r

rotateL :: String -> Integer -> Map -> Map -> Map
rotateL k x l r@(Bin _ _ _ ly ry) =
  if (size ly < ratio * size ry)
     then singleL k x l r
     else doubleL k x l r
rotateL _ _ _ _ = error "rotateL Tip"

rotateR :: String -> Integer -> Map -> Map -> Map
rotateR k x l@(Bin _ _ _ ly ry) r =
  if (size ry < ratio * size ly)
      then singleR k x l r
      else doubleR k x l r
rotateR _ _ _ _ = error "rotateR Tip"

singleL :: String -> Integer -> Map -> Map -> Map
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3) = bin k2 x2 (bin k1 x1 t1 t2) t3
singleL _ _ _ _ = error "singleL Tip"

singleR :: String -> Integer -> Map -> Map -> Map
singleR k1 x1 (Bin _ k2 x2 t1 t2) t3 = bin k2 x2 t1 (bin k1 x1 t2 t3)
singleR _ _ _ _ = error "singleR Tip"

doubleL :: String -> Integer -> Map -> Map -> Map
doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleL _ _ _ _ = error "doubleL"

doubleR :: String -> Integer -> Map -> Map -> Map
doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
doubleR _ _ _ _ = error "doubleR"

bin :: String -> Integer -> Map -> Map -> Map
bin k x l r = Bin (size l + size r + 1) k x l r


size :: Map -> Size
size Tip = 0
size (Bin sz _ _ _ _) = sz

empty :: Map
empty = Tip

singleton :: String -> Integer -> Map
singleton k v = Bin 1 k v empty empty

fromList :: [(String, Integer)] -> Map
fromList ((k, v):xs) = insert k v (fromList xs)
fromList _ = empty

toList :: Map -> [(String, Integer)]
toList (Bin _ k v a b) = concat [toList a, [(k, v)], toList b]
toList _ = []

