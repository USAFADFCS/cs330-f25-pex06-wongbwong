-- pex6.hs 
-- unKnot Haskell

-- name: Benjamin Wong

{- DOCUMENTATION: Used Gavin's test cases
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | type1trip tripCode = unKnot (type1move tripCode)
   | type1trip (wrapTrip tripCode) = unKnot (type1move (wrapTrip tripCode))
   | type2trip tripCode = unKnot (type2move tripCode)
   | type2trip (wrapTrip tripCode) = unKnot (type2move (wrapTrip tripCode))
   | otherwise = "tangle – resulting trip code: " ++ (show tripCode)

type1trip :: [(Char, Char)] -> Bool
type1trip [] = False
type1trip [_] = False
type1trip ((c1, t1):(c2, t2):rest)
   | c1 == c2  = True
   | otherwise = type1trip ((c2, t2) : rest)

type1move :: [( Char, Char)] -> [(Char, Char)]
type1move [] = []
type1move [x] = [x]
type1move ((c1, t1):(c2, t2):rest)
   | c1 == c2  = rest                   
   | otherwise = (c1, t1) : type1move ((c2, t2) : rest)

type2trip :: [(Char, Char)] -> Bool
type2trip [] =  False
type2trip [_] = False
type2trip ((c1,t1):(c2, t2) : rest)
   | t1 == t2 && type2pair2 c1 c2 rest = True
   | otherwise = type2trip ((c2, t2):rest)

type2pair2 :: Char -> Char -> [(Char, Char)] -> Bool
type2pair2 _ _ [] = False
type2pair2 _ _ [_] = False
type2pair2 c1 c2 ((c3, t3):(c4, t4) : rest)
   | t3 == t4 && match c1 c2 c3 c4 = True
   | otherwise = type2pair2 c1 c2 ((c4, t4):rest)

type2move :: [(Char, Char)] -> [(Char, Char)] 
type2move [] = []
type2move [x] = [x]
type2move ((c1,t1):(c2,t2) : rest)
   | t1 == t2 && type2pair2 c1 c2 rest =
        type2remove2 c1 c2 rest
   | otherwise =
        (c1,t1) : type2move ((c2,t2):rest)


type2remove2 :: Char -> Char -> [(Char, Char)] -> [(Char, Char)]
type2remove2 _ _ [] = []
type2remove2 _ _ [x] = [x]
type2remove2 c1 c2 ((c3, t3):(c4 ,t4) : rest)
   | t3 == t4 && match c1 c2 c3 c4 = rest
   | otherwise = (c3,t3) : type2remove2 c1 c2 ((c4,t4) : rest)

match :: Char -> Char -> Char ->  Char -> Bool
match c1 c2 c3 c4 =(c1 == c3 && c2 == c4)

wrapTrip :: [(Char, Char)] -> [(Char, Char)]
wrapTrip [] = []
wrapTrip (x:xs) = xs ++ [x]



main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print ("   test case t01 - tripcode: " )
   print t01
   print ("   result:" ++ unKnot t01)

   let t02 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print ("   test case t02 - tripcode: " ) -- "Unkot"
   print t02
   print ("   result:" ++ unKnot t02) -- "Unkot"

   let t03 = [('a','u'),('b','u'),('a','o'),('b','o')]
   print ("   test case t03 - tripcode: " )
   print t03
   print ("   result:" ++ unKnot t03) -- "Unkot"

   let t04 = [('a','o'),('b','u'),('a','u'),('b','o')]
   print ("   test case t04 - tripcode: " )
   print t04
   print ("   result:" ++ unKnot t04) -- "Unkot"

   let t05 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),
              ('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),
              ('g','u'),('h','o'),('e','o'),('c','o')]
   print ("   test case t05 - tripcode: " )
   print t05
   print ("   result:" ++ unKnot t05)

   let t06 = [('a','o'),('q','u'),('a','u')]
   print ("   test case t06 - tripcode: " )
   print t06
   print ("   result:" ++ unKnot t06) -- [('q','u')]

   let t07 = [('a','o'),('a','u'),('q','u')]
   print ("   test case t07 - tripcode: " )
   print t07
   print ("   result:" ++ unKnot t07) -- [('q','u')]

   let t08 = [('a','o'),('b','o'),('a','u'),('b','u'),('q','u')]
   print ("   test case t08 - tripcode: " )
   print t08
   print ("   result:" ++ unKnot t08) -- [('q','u')]

   let t09 = [('a','u'),('b','o'),('a','o'),('b','u'),('q','u')]
   print ("   test case t09 - tripcode: " )
   print t09
   print ("   result:" ++ unKnot t09) -- [('a','o'),('b','o'), ('a','u'), ('b','u') ('q','u')]

   let t10 = [('a','u'),('b','o'),('a','o'),('b','u'),('q','u'),('c','o'),('c','u')]
   print ("   test case t10 - tripcode: " )
   print t10
   print ("   result:" ++ unKnot t10) -- [('a','o'),('b','o'), ('a','u'), ('b','u') ('q','u')]

   let t11 = [('a','u'),('b','o'),('a','o'),('q','u'),('b','u'),('c','o'),('c','u')]
   print ("   test case t11 - tripcode: " )
   print t11
   print ("   result:" ++ unKnot t11) -- [('q','u')]
