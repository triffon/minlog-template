split :: (a,b) -> (a -> b -> c) -> c
split (s,t) f = f s t

data N = O | S N
data B = Ff | Tt
  deriving (Show, Read, Eq, Ord, Enum)


cases :: B -> a -> a -> a
cases Tt s _ = s
cases Ff _ t = t

r :: N -> a -> (N -> a -> a) -> a
r O     s _ = s
r (S n) s t = t n (r n s t)

--------------------

fromN :: N -> Integer
fromN O     = 0
fromN (S n) = succ $ fromN n

toN :: Integer -> N
toN 0 = O
toN n = S $ toN $ pred n

--------------------

z :: N -> B
z = \n -> r n Tt (\_ _ -> Ff)

p :: N -> N
p = \n -> r n O const -- (\n' _ -> n')

--------------------

plus :: N -> N -> N
plus = \m n -> r m n (const S)

plus2 :: N -> N -> N
plus2 = \m -> r m id (const (S .))

eq :: N -> N -> B
eq = \m -> r m z (\_ p n -> r n Ff (\n' _ -> p n'))

mult :: N -> N -> N
mult = \m -> r m (const O) (\m' p n -> plus (p n) n)

mult2 :: N -> N -> N
mult2 = \m n -> r m O (\m' p -> plus p n)

fact :: N -> N
fact = \n -> r n (S O) (\n' p -> mult p (S n'))
