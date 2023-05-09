split :: (a,b) -> (a -> b -> c) -> c
split (s,t) f = f s t

data N = O | S N
data B = Tt | Ff
  deriving (Show, Read, Eq, Ord, Enum)

p :: N -> N
p O     = O
p (S n) = n

z :: N -> B
z O     = Tt
z (S _) = Ff

cases :: B -> a -> a -> a
cases Tt s _ = s
cases Ff _ t = t

y :: (a -> a) -> a
y t = t (y t)

--------------------

fromN :: N -> Integer
fromN O     = 0
fromN (S n) = succ $ fromN n 

toN :: Integer -> N
toN 0 = O
toN n = S $ toN $ pred n 

--------------------

gplus :: (N -> N -> N) -> N -> N -> N
gplus = \f m n -> cases (z m) n (S (f (p m) n))

plus :: N -> N -> N
plus = y gplus

gplus2 :: N -> (N -> N) -> N -> N
gplus2 = \n f m -> cases (z m) n (S (f (p m)))

plus2 :: N -> N -> N
plus2 = y . gplus2

gmult :: N -> (N -> N) -> N -> N
gmult = \n f m -> cases (z m) O (plus (f (p m)) n)  

mult :: N -> N -> N
mult = y . gmult

gfact :: (N -> N) -> N -> N
gfact = \f n -> cases (z n) (S O) (mult n (f (p n)))

fact :: N -> N
fact = y gfact

gammaeq :: (N -> N -> B) -> N -> N -> B
gammaeq = \f m n -> cases (z m) (z n) (f (p m) (p n))

eq :: N -> N -> B
eq = y gammaeq
