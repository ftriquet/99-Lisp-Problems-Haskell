import System.Random
-- Problem 1
myLast :: (Show a, Eq a) =>  [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: (Show a, Eq a) => [a] -> [a]
myButLast [] = []
myButLast [x] = []
myButLast (x:xs) = myButLast' [x] xs
    where myButLast' xs [] = xs
          myButLast' xs [x] = xs
          myButLast' ys (x:xs) = myButLast' (ys ++ [x]) xs

-- Problem 3
elementAt :: (Show a, Eq a) => Int -> [a] -> Maybe a
elementAt _ [] = Nothing
elementAt 0 (x:_) = Just x
elementAt i (x:xs) = elementAt (i - 1) xs

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = myLength' 1 xs
    where myLength' l [] = l
          myLength' l (x:xs) = myLength' (l + 1) xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs)  = myReverse' [x] xs
    where myReverse' xs (y:ys) = myReverse' (y:xs) ys

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = x == (last xs) && (isPalindrome (init xs))

-- Problem 7
data List a = Elem a | List [List a] deriving (Show)

flatten :: (Show a) => List a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List l) = flatten' [] l
    where flatten' :: [b] -> [List b] -> [b]
          flatten' res [] = res
          flatten' res ((Elem x):xs) = flatten' (res ++ [x]) xs
          flatten' res ((List s):xs) = flatten' res (s ++ xs)

-- Problem 8
compress :: [Char] -> [Char]
compress [] = []
compress [x] = [x]
compress (x:xs) = compress' x xs [x]
    where compress' y (x:xs) res
              | y == x = compress' x xs res
              | otherwise = compress' x xs (res ++ [x])
          compress' y [] res = res

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack []   = []
pack (xs) = pack' [] xs
    where pack' res []     = res
          pack' res list@(x:xs) = pack' (res ++ [begin]) end
              where (begin, end) = span (\e -> e == x) list

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map toTuple (pack xs)
    where toTuple (x:xs) = ((length xs + 1), x)

-- Problem 11
data CoefValue a = Single a
                 | Multiple Int a
                 deriving (Show, Eq)

encodeModified :: (Eq a) => [a] -> [CoefValue a]
encodeModified xs = map toCoef (pack xs)
    where toCoef (x:xs) =
              let len = length xs + 1
                  in if len == 1
                        then Single x
                        else Multiple len x

-- Problem 12
decodeModified :: (Eq a) => [CoefValue a] -> [a]
decodeModified xs = decodeModified' [] xs
    where decodeModified' res []     = res
          decodeModified' res (x:xs) = decodeModified' (res ++ decoded)  xs
              where decoded =
                        case x of
                             Single v     -> [v]
                             Multiple n v -> take n (repeat v)

-- Problem 13
encodeDirect :: (Eq a) => [a] -> [CoefValue a]
encodeDirect xs = encodeDirect' [] xs
    where
        encodeDirect' res []     = res
        encodeDirect' res (x:xs) = encodeDirect' (res ++ group : []) remaining
            where (_group, remaining) = span (\e -> e == x) xs
                  groupLen            = length _group + 1
                  group               = if groupLen > 1
                                           then Multiple groupLen x
                                           else Single x

-- Problem 14
dupli :: [a] -> [a]
dupli xs = repli xs 2

-- Problem 15
repli :: [a] -> Int -> [a]
repli _   0 = []
repli []  n = []
repli xs  n = repli' [] xs n
    where repli' res []     _ = res
          repli' res (x:xs) n = repli' (res ++ (take n . repeat) x) xs n

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' 1 [] xs n
    where dropEvery' cur res []     _ = res
          dropEvery' cur res (x:xs) _n
              | cur == _n = dropEvery' 1 res xs _n
              | otherwise = dropEvery' (cur + 1) (res ++ x : []) xs _n

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs beg end = drop (beg - 1) . take end $ xs

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n | n < 0 = rotate xs (-n)
            | n == 0 = xs
            | otherwise = rotate' [] xs n
    where rotate' res xs     0 = xs ++ res
          rotate' res []     n = rotate' [] res n
          rotate' res (x:xs) n = rotate' (res ++ [x]) xs (n -1)

-- Problem 20
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt 0 xs = (Nothing, xs)
removeAt n xs = removeAt' Nothing [] xs n
    where removeAt' _ pre []     _ = (Nothing, pre)
          removeAt' _ pre (x:xs) 1 = (Just x, pre ++ xs)
          removeAt' _ pre (x:xs) n = removeAt' Nothing (pre ++ [x]) xs (n - 1)

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = pre ++ [x] ++ post
    where (pre, post) = Main.split xs n

-- Problem 22
range :: Int -> Int -> [Int]
range beg end = range' [] beg end
    where range' res beg end
              | beg > end  = []
              | beg == end = res ++ [beg]
              | otherwise  = range' (res ++ [beg]) (beg + 1) end

-- Problem 23
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect xs n = rndSelect' (return []) xs n
    where rndSelect' res _  0 = res
          rndSelect' res xs n = do
              x <- (getStdRandom (randomR (0, (length xs) - 1)))
              let value = xs!!x
                  in rndSelect' (fmap (\ys -> ys ++ [value]) res) xs (n - 1)

-- Problem 24
diffSelect :: Int -> Int -> IO [Int]
diffSelect n bound = diffSelectGeneric n [1..bound]


diffSelectGeneric :: (Eq a) => Int -> [a] -> IO [a]
diffSelectGeneric 0 _ = return []
diffSelectGeneric n range = diffSelect' n range []
    where diffSelect' _ [] res = return res
          diffSelect' 0 _  res = return res
          diffSelect' n range res = do
              index <- (getStdRandom (randomR (0, (length range) - 1)))
              let safeInit :: [a] -> [a]
                  safeInit [] = []
                  safeInit xs = init xs
                  val = range!!index
                  (beg, (_:end)) = break (\e -> e == val) range
                  in diffSelect' (n - 1) (beg ++ end) (res ++ [val])

-- Problem 25
rndPermu :: (Eq a) =>  [a] -> IO [a]
rndPermu [] = return []
rndPermu xs = diffSelectGeneric (length xs) xs


main :: IO ()
main = undefined
