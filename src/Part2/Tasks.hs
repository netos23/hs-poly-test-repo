module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 6 |+| 
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 6 |-| 
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times
infixl 7 |*| 


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (IntConstant val) = IntConstant val
replaceVar varName replacement (Variable var)
   | varName == var = replacement
   | otherwise = Variable var
replaceVar varName replacement (BinaryTerm op lhv rhv) = 
   BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)


-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm op lhv rhv) = let 
   a = evaluate lhv
   b = evaluate rhv
   in case (a, b) of
      (IntConstant a, IntConstant b) -> case op of
         Plus -> IntConstant (a + b)
         Minus -> IntConstant (a - b)
         Times -> IntConstant (a * b)
      _ -> BinaryTerm op lhv rhv
evaluate term = term
