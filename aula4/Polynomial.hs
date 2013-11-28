module Polynomial where

-- a polynomial is represented by its coefficients

type Polynomial = [Int]


-- getting the grade of a polynomial

grade :: Polynomial -> Int
grade p = (length (dropWhile (== 0) p)) - 1

-- normalizing a polynomial

normalize :: Polynomial -> Polynomial -> (Polynomial, Polynomial)
normalize p1 p2 = let
					g1 = grade p1
					g2 = grade p2
					complete l n = foldl (\ac _ -> 0:ac) l [1..n]
                  in if g1 > g2 then (p1, complete p2 (g1 - g2))
                       else (complete p1 (g2 - g1), p2)

-- summing two polynomials

(.+.) :: Polynomial -> Polynomial -> Polynomial
p1 .+. p2 = uncurry (zipWith (+)) (normalize p1 p2)

-- multiplying two polynomials

(.*.) :: Polynomial -> Polynomial -> Polynomial
p1 .*. p2 = undefined

-- evaluate a polynomial

eval :: Int -> Polynomial -> Int
eval x = fst . foldr step  (0,1)
         where
            step t (r,vn) = (t * vn + r, vn * x)


-- the derivative of a polynomial

derivative :: Polynomial -> Polynomial
derivative = snd . foldr step (1,[]) . init
             where
                step x (e, p) = (e + 1, (x * e) : p)

-- converting a polynomial into a string

showPolynomial :: Polynomial -> String
showPolynomial = snd . foldr step (0,[])
                 where
                    step t (e,ac) = (e + 1, showt t e <> ac)
                    showt x 0 = show x
                    showt x y = show x ++ "x^" ++ show y
                    v <> [] = v
                    v <> z = v ++ " + " ++ z

-- exemple polynomial

poly :: Polynomial
poly = [3,2,1]

poly1 :: Polynomial
poly1 =  [4, 2, 0, -5]
