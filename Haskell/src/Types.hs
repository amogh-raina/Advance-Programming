-- Do not change anything in this file!
module Types where

-- Abstract syntax

data Exp =
    Cst Int
  | Var VName
  | Join [Exp]
  | Let VName Exp (Maybe Exp)
  | Seq Exp Exp
  | Is Exp Exp
  | Sum Exp
  | Count Exp
  | Times Exp Exp
  | Roll Exp
  | Take Sel VName
  deriving (Eq, Show)

type VName = String

data Sel = Min | Max | Rand
  deriving (Eq,Show)

-- Calculation

type Prob = Rational -- must be >=0 and <=1

newtype PD a = PD {runPD :: [(Prob, a)]}  -- all probs must be >0, and sum to 1
  deriving Show

data Bag = Bag {contents :: [Int]} -- must be ordered
  deriving (Eq, Ord, Show)

data RunError =
    Unbound VName Info
  | NonSingleton Info
  | EmptyChoice Info
  | OtherError Info -- anything not covered by the above
  deriving (Show, Eq, Ord)

type Info = String -- optional additional information about the error
