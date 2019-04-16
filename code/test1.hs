maxi :: (Int,Int)->Int
maxi (x,y) | x>=y = x
           | otherwise = y

plus :: (Int,Int)->Int
plus (x,y) = x+y

plus1 :: Int->Int->Int
plus1 x y = x+y

fact :: Int->Int
fact 0 = 1
fact n = n*fact (n-1)

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  

pat1 [x] = True

pat2 [[x]] = True

--a)
myrem :: Int -> Int -> Int
myrem x 0 = x
myrem x y 
     | y < 0 = myrem x (-y)
     | x < y = x
     | x-y >= y = myrem (x-y) y
     | otherwise = x-y

--b)
count :: Int -> [ Int ] -> Int
count n [] = 0
count n (x:xs)
     |  n == x = 1 + count n xs
     |  otherwise = count n xs

--c)
simplify :: [ Int ] -> [( Int , Int )]
simplify [] = []
simplify (x:xs) = (x, count x (x:xs)):simplify(remove x xs)

remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove n (x:xs) 
     | n == x = remove n xs
     | otherwise = x:remove n xs

--d)
multUnion :: [( Int , Int )] -> [( Int , Int )] -> [( Int , Int )]
multUnion [] [] = []
multUnion (x:xs) [] = x:multUnion xs []
multUnion [] (y:ys) = y:multUnion [] ys
multUnion (x:xs) (y:ys)
		 | a == c = (a,b+d):multUnion xs ys
         | a < c = (a,b):multUnion(xs)(y:ys)
		 | otherwise = (c,d):multUnion(x:xs)(ys)
		 where (a,b) = x
		       (c,d) = y
		 
checkOrder :: [( Int , Int )] -> Bool
checkOrder [] = True
checkOrder (_:[]) = True
checkOrder (x:y:xs)
          | x <= y = checkOrder (y:xs)
		  | otherwise = False
	 
--simplify (x:xs) = simplify(outer(x:xs))
--[(u,count u (x:xs)) | u <- outer (x:xs)]

{-unique :: [Int] -> [Int]
unique [] = []
unique (x:xs) 
      | isIn x xs = unique xs
      | otherwise = x:unique xs

isIn :: Int -> [Int] -> Bool
isIn _ [] = False
isIn n (x:xs) 
    | n == x = True
    | otherwise = isIn n xs
-}


		  
		  
(^^^) :: [Int] -> [Int] -> Int
[] ^^^ [] = 1 
(x:xs) ^^^ (y:ys)
		   | y >= 0 = x `r2` y*xs ^^^ ys
		   | otherwise = xs ^^^ ys


r2 :: Int -> Int -> Int
r2 0 0 = 1
r2 b 0 = 1
r2 b n = b*r2 b (n-1)


{-outer :: [Int] -> [(Int,Int)]
outer [] = []
outer (x:xs) = (x, count x (x:xs)):outer(inner x xs)

inner :: Int -> [Int] -> [Int]
inner _ [] = []
inner n (x:xs) 
     | n == x = inner n xs
     | otherwise = x:inner n xs
-}