-- {-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module NaturalNumber where

data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat

type family Add n m where
    Add 'Zero m = m
    Add ('Succ a) m = 'Succ (Add a m)

type family Product n m where
    Product ('Succ 'Zero) m = m
    Product ('Succ a) m = Add m (Product a m)
