




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
	

simplify :: (Term) -> Term
	simplify (Sum (fromInteger 0) t) = simplify t
	simplify (Sum t (fromInteger 0)) = simplify t
	simplify (Mult (fromInteger 1) t) = simplify t
	simplify (Mult t (fromInteger 1)) = simplify t
	simplify (Exp t (fromInteger 0)) = fromInteger 1

	-- FALTA


distrib :: (Term) -> Term
	distrib (Mult t1 (Sum t2 t3)) = Sum (Mult t1 t2) (Mult t1 t3)
	distrib (Mult (Sum t2 t3) t1 = Sum (Mult t2 t1) (Mult t3 t1)