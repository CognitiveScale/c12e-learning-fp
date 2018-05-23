module HW4.Problem where

{-

Part 1: What is the kind of each of the following data types?

-}

data Pair a b = Pair a b

data Eyether a b = LeftEye a | RightEye b

newtype Function a b =
    Function { getFunction :: a -> b }

newtype Cont r a =
    Cont { runCont :: (a -> r) -> r }

newtype Mystery r a =
    Mystery { unravelMystery :: (a -> r) -> a }

data Compose f g a =
    Compose { getCompose :: f (g a) }

data Expr a
    = Var a
    | Lit Int
    | Add (Expr a) (Expr a)


{-

Part 2: Define Functor instances for all of the data types above.
Verify that the laws hold:

"""
The Functor class is used for types that can be mapped over. Instances
of Functor should satisfy the following laws:

     fmap id  ==  id                                 (identity)
fmap (f . g)  ==  fmap f . fmap g    for all f, g    (associativity)
"""

-}


instance Functor (Pair a) where
    fmap = undefined


instance Functor (Eyether a) where
    fmap = undefined


instance Functor (Function a) where
    fmap = undefined


instance Functor (Cont r) where
    fmap = undefined


instance Functor (Mystery r) where
    fmap = undefined


instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap = undefined


instance Functor Expr where
    fmap = undefined
