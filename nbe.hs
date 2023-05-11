import Data.Unique
genSym :: String -> IO String
genSym name = ((name ++) . show . hashUnique) <$> newUnique

data T = TVar String | T :⇒ T
  deriving (Eq, Show, Read, Ord)
data Λ = Var String | Λ :@ Λ | Abs String Λ
  deriving (Eq, Show, Read, Ord)
-- S = [[Λ]]
data S = Atom Λ | Fun { getFun :: S -> IO S }

type Valuation = String -> S

modify :: Valuation -> (String, S) -> Valuation
modify ξ (x,a) y
  | y == x    = a
  | otherwise = ξ y

evaluate :: Λ -> Valuation -> IO S
evaluate (Var x)    ξ = return $ ξ x
evaluate (m₁ :@ m₂) ξ = do
  a <- evaluate m₁ ξ
  b <- evaluate m₂ ξ
  getFun a b
evaluate (Abs x n)  ξ = return $ Fun $ \a -> evaluate n $ modify ξ (x,a)

(⇑) :: Λ -> T -> IO S
m ⇑ (TVar μ) = return $ Atom m
m ⇑ (ρ :⇒ σ) = do
  let f a = do
        n <- a ⇓ ρ
        (m :@ n) ⇑ σ
  return $ Fun f

(⇓) :: S -> T -> IO Λ
(Atom m) ⇓ (TVar μ) = return m
(Fun a)  ⇓ (ρ :⇒ σ) = do
  x <- genSym "x"
  xx <- (Var x) ⇑ ρ
  y <- a xx
  n <- (y ⇓ σ)
  return $ Abs x n

nbe :: Λ -> T -> IO Λ
nbe m τ = do
  val <- evaluate m undefined
  val ⇓ τ

k = Abs "x" $ Abs "y" $ Var "x"
s = Abs "x" $ Abs "y" $ Abs "z" (((Var "x") :@ (Var "z")) :@ ((Var "y") :@ (Var "z")))

i = nbe ((s :@ k) :@ k) (TVar "α" :⇒ TVar "α")

