import Data.Unique
genSym :: String -> IO String
genSym name = ((name ++) . show . hashUnique) <$> newUnique

data T = TVar String | T :⇒ T
data Λ = Var String | Λ :@ Λ | Abs String Λ
-- S = [[Λ]]
data S = Atom Λ | Fun { getFun :: S -> S }

type Valuation = String -> S

modify :: Valuation -> (String, S) -> Valuation
modify ξ (x,a) y
  | y == x    = a
  | otherwise = ξ y

evaluate :: Λ -> Valuation -> S
evaluate (Var x)    ξ = ξ x
evaluate (m₁ :@ m₂) ξ = getFun (evaluate m₁ ξ) $ evaluate m₂ ξ
evaluate (Abs x n)  ξ = Fun $ \a -> evaluate n $ modify ξ (x,a)

(⇑) :: Λ -> T -> S
m ⇑ (TVar μ) = Atom m
m ⇑ (ρ :⇒ σ) = Fun $ \a -> (m :@ (a ⇓ ρ)) ⇑ σ

(⇓) :: S -> T -> Λ
(Atom m) ⇓ (TVar μ) = m
(Fun a)  ⇓ (ρ :⇒ σ) = Abs "x" ((a ((Var "x") ⇑ ρ)) ⇓ σ)

nbe :: Λ -> T -> Λ
nbe m τ = (evaluate m undefined) ⇓ τ
