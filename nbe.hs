import Data.Unique
import Control.Arrow
import Control.Monad
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
evaluate (m₁ :@ m₂) ξ = join $ getFun <$> evaluate m₁ ξ <*> evaluate m₂ ξ
evaluate (Abs x n)  ξ = return $ Fun $ \a -> evaluate n $ modify ξ (x,a)

(⇑) :: Λ -> T -> IO S
m ⇑ (TVar μ) = return $ Atom m
m ⇑ (ρ :⇒ σ) = return $ Fun $ (⇑σ) . (m:@) <=< (⇓ρ)

(⇓) :: S -> T -> IO Λ
(Atom m) ⇓ (TVar μ) = return m
(Fun a)  ⇓ (ρ :⇒ σ) = genSym "x" >>= \x -> (return . Abs x <=< (⇓σ) <=< a) =<< Var x ⇑ ρ

nbe :: Λ -> T -> IO Λ
nbe m τ = (⇓ τ) =<< evaluate m undefined

k = Abs "x" $ Abs "y" $ Var "x"
s = Abs "x" $ Abs "y" $ Abs "z" (((Var "x") :@ (Var "z")) :@ ((Var "y") :@ (Var "z")))

i = nbe ((s :@ k) :@ k) (TVar "α" :⇒ TVar "α")

