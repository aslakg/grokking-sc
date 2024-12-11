module Core.TryABT where

import Core.ABT
import Prelude hiding (abs)

{- Okay let's try to play with this and see what we can do. -}
--   data Producer = Literal Integer
data Color a = Red | Green
  deriving (Functor, Foldable)

xvar :: Term f
xvar = var "x"

-- | So I couldn't do tm Red without first deriving Functor AND Foldable
foo :: Term Color
foo = tm Red

-- Define a functor for a simple arithmetic language
data ArithF a
  = Lit Int
  | Add a a
  | Mul a a
  deriving (Functor, Foldable, Show)

-- Now you can build terms using tm
litTerm :: Int -> Term ArithF
litTerm n = tm (Lit n)

addTerm :: Term ArithF -> Term ArithF -> Term ArithF
addTerm t1 t2 = tm (Add t1 t2)

mulTerm :: Term ArithF -> Term ArithF -> Term ArithF
mulTerm t1 t2 = tm (Mul t1 t2)

absTerm v t1 = abs v t1

ex2 :: Term ArithF
ex2 = abs "x" $ (mulTerm (var "x") (var "hah"))

-- Example usage:
-- (2 + 3) * 4
example :: Term ArithF
example =
  mulTerm
    (addTerm (litTerm 2) (litTerm 3))
    (litTerm 4)

incoming = abs "y" (addTerm (var "y") (var "x"))

ex2s :: Term ArithF
ex2s =
  subst
    (litTerm 4)
    -- incoming
    "hah"
    ex2

foo1 :: (Show s) => s -> String
foo1 = show

instance Show (Term ArithF) where
  show :: Term ArithF -> String
  show t = case out t of
    Var x -> x
    Abs v body -> "λ" ++ v ++ "." ++ show body
    Tm t -> show t

-- | Very weirdly, this gives foo2:λx.Mul Lit 4 hah , even though I'm replacing hah, not x
foo2 :: Term ArithF
foo2 = ex2s

data Mini a
  = App a a
  | Msg String
  deriving (Foldable, Functor)

tm1 :: Term Mini
tm1 =
  tm $ Msg "Hey"

-- | This is where it goes wrong, the type is explicitly listed in Elm, while in Haskell it's just Term Mini
tm2 :: Term Mini
tm2 =
  tm (App tm1 (tm (Msg "Stop")))

fmapM f m =
  case m of
    App a b ->
      App (f a) (f b)
    Msg s ->
      Msg s