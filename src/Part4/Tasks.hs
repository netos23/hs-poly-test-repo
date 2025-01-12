{-# LANGUAGE InstanceSigs #-}
module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl (:<) REmpty

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec :: Show a => Int -> ReverseList a -> ShowS
    showsPrec _ REmpty = showString ""
    showsPrec d (init :< last) = showString $ "[" ++ showInner init ++ show last ++ "]" where 
        showInner REmpty = ""
        showInner (init :< last) = showInner init ++ show last ++ ","
    show REmpty = "[]"
    show (init :< last) = "[" ++ showInner init ++ show last ++ "]" where 
        showInner REmpty = ""
        showInner (init :< last) = showInner init ++ show last ++ ","
            
instance Eq a => Eq (ReverseList a) where
    (==) :: Eq a => ReverseList a -> ReverseList a -> Bool
    REmpty == REmpty = True
    (init1 :< last1) == (init2 :< last2) = init1 == init2 && last1 == last2
    _ == _ = False

    (/=) :: Eq a => ReverseList a -> ReverseList a -> Bool
    a /= b = not (a == b)
instance Semigroup (ReverseList a) where
    (<>) :: ReverseList a -> ReverseList a -> ReverseList a
    REmpty <> a = a
    a <> REmpty = a 
    a <> (init :< last) = (a <> init) :< last

instance Monoid (ReverseList a) where
    mempty = REmpty
instance Functor ReverseList where
    fmap :: (a -> b) -> ReverseList a -> ReverseList b
    fmap f REmpty = REmpty
    fmap f (init:<last) = fmap f init :< f last
instance Applicative ReverseList where
    pure :: a -> ReverseList a
    pure x = REmpty :< x
    (<*>) :: ReverseList (a -> b) -> ReverseList a -> ReverseList b
    REmpty <*> _ = REmpty
    _ <*> REmpty = REmpty
    (fs :< f) <*> xs = (fs <*> xs) <> (f <$> xs)
instance Monad ReverseList where
    REmpty >>= _ = REmpty
    (xs :< x) >>= f = (xs >>= f) <> f x