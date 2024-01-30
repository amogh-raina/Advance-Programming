-- Put your calculator implementation in this file
-- Suppress warning for old-style monad definitions
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module CalculatorImpl where

import Types

import Control.Monad
import Data.Ratio ((%))

-- The type constructor PD is defined in Types
instance Monad PD where
  return x = PD [(1, x)]
  (PD xs) >>= f = PD $ concatMap (\(p, x) -> scaleProb p (runPD (f x))) xs
    where
      scaleProb p ys = [(p * q, y) | (q, y) <- ys]


-- You shouldn't need to modify these
instance Applicative PD where
  pure = return; (<*>) = ap
instance Functor PD where
  fmap = liftM

-- | `uniform` generates a uniform probability distribution for the integers from 0 to n-1.
-- If n is zero or negative, it will produce an error as a uniform distribution is not defined for these cases.
uniform :: Int -> PD Int
uniform n
  | n > 0 = PD [(1 % fromIntegral n, x) | x <- [1..n]]
  | otherwise = error "uniform: non-positive argument"


normalize :: Ord a => PD a -> PD a
normalize (PD xs) = PD $ map normalizeProb $ accumulate xs
  where
    -- accumulate goes through each (probability, outcome) pair and accumulates probabilities for each unique outcome
    accumulate :: Ord a => [(Prob, a)] -> [(Prob, a)]
    accumulate [] = []
    accumulate ((p, x):ys) = (totalP, x) : accumulate (filter ((/= x) . snd) ys)
      where
        totalP = p + sum [pi | (pi, xi) <- ys, xi == x]

    -- normalizeProb adjusts each probability to ensure the total is 1
    normalizeProb :: (Prob, a) -> (Prob, a)
    normalizeProb (p, x) = (p / totalProb, x)

    -- Calculate the total probability of the original list
    totalProb = sum $ map fst xs

expectation :: Fractional a => PD (Maybe a) -> (Prob, a)
expectation (PD xs) = if successProb == 0
                      then error "Expected value is undefined"
                      else (successProb, weightedAvg)
  where
    justVals = [(p, x) | (p, Just x) <- xs]
    successProb = sum $ map fst justVals
    weightedAvg = sum [fromRational p * x | (p, x) <- justVals] / fromRational successProb


-- The type Bag is defined in Types
union :: Bag -> Bag -> Bag
union (Bag xs) (Bag ys) = Bag (merge xs ys)
  where
    merge [] bs = bs
    merge as [] = as
    merge (a:as) (b:bs)
      | a < b     = a : merge as (b:bs)
      | otherwise = b : merge (a:as) bs

type Value = Bag  -- all elements must be >=0

type Store = [(VName,Value)] -- all vnames must be distinct

newtype Calc a = Calc {runCalc :: Store -> PD (Either RunError (a,Store))}

instance Monad Calc where
  return x = Calc $ \s -> PD [(1, Right (x, s))]
  (Calc c) >>= f = Calc $ \s -> PD $ do
      (prob, result) <- runPD (c s)
      case result of
          Left err -> [(prob, Left err)]
          Right (a, s') -> runPD $ runCalc (f a) s'

-- You shouldn't need to modify these
instance Applicative Calc where
  pure = return; (<*>) = ap
instance Functor Calc where
  fmap = liftM

fault :: RunError -> Calc a
fault re = Calc $ \_ -> PD [(1, Left re)]

lookVar :: VName -> Calc (Maybe Value)
lookVar x = Calc $ \s -> PD [(1, Right (lookup x s, s))]

setVar :: VName -> Maybe Value -> Calc ()
setVar x mv = Calc $ \s -> PD [(1, Right ((), updateStore x mv s))]
  where
    updateStore :: VName -> Maybe Value -> Store -> Store
    updateStore varName maybeVal store =
      case maybeVal of
        Just val -> (varName, val) : filter ((varName /=) . fst) store
        Nothing  -> filter ((varName /=) . fst) store

pick :: PD a -> Calc a
pick pd = Calc $ \s -> PD $ concatMap (\(prob, val) -> [(prob, Right (val, s))]) (runPD pd)

eval :: Exp -> Calc Value
eval (Cst i)       = return $ Bag [i] -- Constant integers are represented as singleton bags.
eval (Var v)       = do -- Look up a variable and return its value.
  maybeValue <- lookVar v
  case maybeValue of
    Just value -> return value
    Nothing -> fault $ Unbound v "Variable not found"
eval (Join exps)   = do -- Evaluate each expression and join their results.
  bags <- mapM eval exps
  return $ foldr union (Bag []) bags
eval (Let v e1 me2) = do -- Evaluate e1, bind its value to v, then evaluate me2 if present.
  val1 <- eval e1
  setVar v (Just val1)
  case me2 of
    Just e2 -> eval e2
    Nothing -> return val1
eval (Seq e1 e2) = eval e1 >> eval e2
eval (Is e1 e2) = do
  val1 <- eval e1
  val2 <- eval e2
  return $ if val1 == val2 then Bag [1] else Bag []
eval (Sum e) = do
  Bag ints <- eval e
  return $ Bag [sum ints]
eval (Count e) = do
  Bag ints <- eval e
  return $ Bag [length ints]
eval (Times e1 e2) = do
  Bag ints1 <- eval e1
  Bag ints2 <- eval e2
  return $ Bag [x * y | x <- ints1, y <- ints2]
eval (Roll e) = do
  Bag ints <- eval e
  if null ints then fault $ EmptyChoice "Cannot roll an empty bag"
  else do
    let n = head ints
    if n <= 0 then
        fault $ OtherError "Cannot roll a die with non-positive sides"
    else do
        rolledNumber <- pick $ uniform n
        return $ Bag [rolledNumber]
eval (Take sel v) = do
  maybeVal <- lookVar v
  case maybeVal of
    Just (Bag ints) -> case sel of
      Min -> return . Bag $ [minimum ints | not (null ints)]
      Max -> return . Bag $ [maximum ints | not (null ints)]
      Rand -> if null ints then fault $ EmptyChoice "Cannot pick from an empty bag"
              else pick $ uniform (length ints) >>= \i -> return $ Bag [ints !! i]
    Nothing -> fault $ Unbound v "Variable not found in Take"


evaluate :: Exp -> PD (Either RunError Value)
evaluate exp = normalize $ PD $ concatMap processOutcome (runPD outcomesPD)
  where
    initialStore = [] :: Store
    outcomesPD = runCalc (eval exp) initialStore
    processOutcome :: (Prob, Either RunError (Value, Store)) -> [(Prob, Either RunError Value)]
    processOutcome (prob, Right (val, _)) = [(prob, Right val)]
    processOutcome (prob, Left err) = [(prob, Left err)]
