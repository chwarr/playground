myReverse :: [a] -> [a]
myReverse lst = reverse' lst []
    where reverse' :: [a] -> [a] -> [a]
          reverse' [] acc = acc
          reverse' (x:xs) acc = reverse' xs (x:acc)

myTake :: Int -> [a] -> [a]
myTake 0 lst = []
myTake n [] = []
myTake n lst = (head lst):(myTake (n - 1) (tail lst))

myDrop :: Int -> [a] -> [a]
myDrop 0 lst = lst
myDrop n [] = []
myDrop n lst = myDrop (n - 1) (tail lst)

myZip :: [a] -> [b] -> [(a,b)]
myZip lstA [] = []
myZip [] lstB = []
myZip (a:as) (b:bs) = (a,b):(myZip as bs)

countUntil :: (Eq a) => [a] -> a -> Int
countUntil [] _ = 0
countUntil (x:xs) needle =
  if x == needle
     then 0
     else 1 + (countUntil xs needle)

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem needle (x:xs) =
  if x == needle
     then True
     else myElem needle xs
