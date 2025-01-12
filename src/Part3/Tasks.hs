module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n+1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

partitionBy :: [b] -> (b -> b -> Bool) -> ([b], b, [b])
partitionBy (h:t) c = let 
    partitionWithPivot  [] (left, pivot, right) = (left, pivot, right)
    partitionWithPivot  (h:t) (left, pivot, right)
        | c pivot h = partitionWithPivot t (left ++ [h], pivot, right)
        | otherwise = partitionWithPivot t (left, pivot, right ++ [h])
    in partitionWithPivot t ([],h,[])


comparing :: Ord a => (t -> a) -> t -> t -> Bool
comparing f a b = f a >= f b

qsort :: Ord a => [a] -> [a]
qsort list = qsortBy list (>=)

qsortBy :: [a] -> (a -> a -> Bool) -> [a]
qsortBy [] c = []
qsortBy list c = 
    let (left,p,right) = partitionBy list c
    in qsortBy left c ++ [p] ++ qsortBy right c

group :: Eq t => [t] -> [[t]]
group list = groupBy list (==)

groupBy :: [t] -> (t -> t -> Bool) -> [[t]]
groupBy list f = let 
    groupInternal [] groups = groups
    groupInternal (x:xs) [] = groupInternal xs [[x]]
    groupInternal (x:xs) (h:t)
        | f x (head h) = groupInternal xs ((h ++ [x]) : t)
        | otherwise = groupInternal xs ([x] : h : t)
    reverseList [] = []
    reverseList (x:xs) = reverseList xs ++ [x]
    in reverseList (groupInternal list [])

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq list = let 
    splitNumber :: Int -> [Int]
    splitNumber x
        | x < 10 = [x]
        | otherwise = splitNumber (x `div` 10) ++ [x `mod` 10]
    maxByLen a b
        | length a <= length b = b
        | otherwise = a
    in head $ foldr maxByLen [] (group $ qsort $ concatMap splitNumber list)


data TreeSet val = Empty | Node val (TreeSet val) (TreeSet val) deriving(Show,Eq)

addNode :: Ord t => TreeSet t -> t -> TreeSet t
addNode Empty val = Node val Empty Empty
addNode (Node v l r) val
    | v == val = Node v l r
    | v < val = Node v (addNode l val) r
    | otherwise = Node v l (addNode r val)

asList :: TreeSet a -> [a]
asList Empty = []
asList (Node v l r) = asList l ++ [v] ++ asList r


fromList :: Ord t => [t] -> TreeSet t
fromList list = let
    parseList [] node = node
    parseList (h:t) node = parseList t (addNode node h)
    in parseList list Empty

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Ord a) => [a] -> [a]
uniq l =  asList (fromList l)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Ord k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = let
    pairs = [(f x, x) | x <- l]
    groups = groupBy (qsortBy pairs (comparing fst)) (\ a b -> fst a == fst b)
    in [(fst (head g), map snd g )  | g <- groups]