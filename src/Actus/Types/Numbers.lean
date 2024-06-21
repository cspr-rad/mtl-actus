structure Money where
  amount : Int
  deriving BEq, Hashable, Repr, DecidableEq

def Money.map (f : Int -> Int) (m : Money) : Money :=
  { amount := f m.amount }

structure Scalar where
  value : Int
  deriving BEq, Hashable, Repr, DecidableEq

def Scalar.map (f : Int -> Int) (s : Scalar) : Scalar :=
  { value := f s.value }
