




data Term = Const Int Int | Var String | Fun String Term | Integ Term Term | Sum Term Term | Mult Term Term | Exp Term Int

t :: Term
t = Var "t"

u :: Term
u = Var "u"

v :: Term
v = Var "v"

w :: Term
w = Var "w"

x :: Term
x = Var "x"

y :: Term
y = Var "y"

z :: Term
z = Var "z"

sen :: (Term) -> Term
sen (t1) = Fun "sen" t1

cosen :: (Term) -> Term
cosen (t1) = Fun "cos" t1

instance Show Term where
	show (Const 0 _) = show 0
	show (Const p 0) = error "Error, el denominador no puede ser cero"
	show (Const p 1) = show p
	show (Const p q) = "\\frac{" ++ show p ++ "}{" ++ show q ++ "}"
	show (Var x) = show x
	show (Sum t1 t2) = show t1 ++ "+" ++ show t2
	show (Mult t1 t2) = show t1 ++ "*" ++ show t2
	show (Fun f t) = show f ++ "(<" ++ show t ++ ")>"
	show (Fun "sen" t) = "sen(" ++ show t ++ ")"
	show (Fun "cos" t) = "cos(" ++ show t ++ ")"
	show (Integ t1 x) = "\\int <" ++ show t1 ++ "> d<" ++ show x ++ ">" 
	show (Exp (Fun "sen" t1) n) = "sen^{" ++ show n ++ "}(<" ++ show t1 ++ ">)"
	show (Exp (Fun "cos" t1) n) = "cos^{" ++ show n ++ "}(<" ++ show t1 ++ ">)"
	show (Exp t1 n) = "<" ++ show t1 ++ ">^{" ++ show n ++ "}"


instance Num Term where
	t1 + t2 = Sum t1 t2
	t1 * t2 = Mult t1 t2
	fromInteger i = Const (fromInteger i) 1

-- FUNCION SIMPLIFICACION
simplify :: (Term) -> Term
simplify (Exp t 0) = Const 1 1
simplify (Exp (Const p q) n) = Const (p ^ n) (q ^ n)
simplify (Const p q) = Const (div p (gcd p q)) (div q (gcd p q)) 
simplify (Sum (Const a b) (Const c d)) = Const ((a * (div (lcm b d) b)) + (c * (div (lcm b d) d))) (lcm b d)
simplify (Sum t (Const 0 1)) = t
simplify (Sum (Const 0 1) t) = t
simplify (Mult (Const 1 1) t) = t
simplify (Mult t (Const 1 1)) = t

-- FUNCION DISTRIBUTIVA
distrib :: (Term) -> Term
distrib (Mult t1 (Sum t2 t3)) = Sum (Mult t1 t2) (Mult t1 t3)
distrib (Mult (Sum t2 t3) t1) = Sum (Mult t2 t1) (Mult t3 t1)

-- FUNCION POTENCIA
pow :: (Term) -> Term
pow (Exp (Const t1 t2) n) = Const (t1 ^ n) (t2 ^ n)
pow (Exp (Exp t n) m) = Exp t (n * m)
pow (Exp (Mult t1 t2) n) = Mult (Exp t1 n) (Exp t2 n)
-- pow (Exp (Sum t1 t2) n) = 

-- FUNCION FACTORIAL
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- FUNCION SUMATORIA
summation :: Integer -> Integer -> Integer
summation x y = summation' x y 0

summation' :: Integer -> Integer -> Integer -> Integer
summation' x y sum =
    if (y<x) then
        sum
    else
        summation' x (y-1) (sum+y)

--Mult (Const (factorial n) (Mult (factorial i) (factorial (n - i)))) (Mult (Exp t1 (n - i)) (Exp t2 i)) 