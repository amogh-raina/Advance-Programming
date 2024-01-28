-- Put your calculator implementation in this file
-- Suppress warning for old-style monad definitions
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module CalculatorImpl where

import Types

import Control.Monad

-- The type constructor PD is defined in Types
instance Monad PD where
  return = undefined
  (>>=) = undefined

-- You shouldn't need to modify these
instance Applicative PD where
  pure = return; (<*>) = ap
instance Functor PD where
  fmap = liftM

uniform :: Int -> PD Int
uniform = undefined

normalize :: Ord a => PD a -> PD a
normalize = undefined

expectation :: Fractional a => PD (Maybe a) -> (Prob, a)
expectation = undefined

-- The type Bag is defined in Types
union :: Bag -> Bag -> Bag
union = undefined

type Value = Bag  -- all elements must be >=0

type Store = [(VName,Value)] -- all vnames must be distinct

newtype Calc a = Calc {runCalc :: Store -> PD (Either RunError (a,Store))}

instance Monad Calc where
  return = undefined
  (>>=) = undefined

-- You shouldn't need to modify these
instance Applicative Calc where
  pure = return; (<*>) = ap
instance Functor Calc where
  fmap = liftM

fault :: RunError -> Calc a
fault = undefined

lookVar :: VName -> Calc (Maybe Value)
lookVar = undefined

setVar :: VName -> Maybe Value -> Calc ()
setVar = undefined

pick :: PD a -> Calc a
pick = undefined

eval :: Exp -> Calc Value
eval = undefined

evaluate :: Exp -> PD (Either RunError Value)
evaluate = undefined
