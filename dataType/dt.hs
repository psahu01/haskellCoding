type Pos = (Int, Int)

dist :: Pos -> Pos -> Int
dist (a,b) (x,y) = (a - x)^2 + (b - y)^2

data Shape = Circle Float
              | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle p q) = p * q

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat (Succ m) n = Succ (addNat m n)

multNat :: Nat -> Nat -> Nat
multNat m Zero = Zero
multNat m (Succ n) = addNat m (multNat m n)