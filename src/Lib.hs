module Lib
    ( someFunc
    ) where


import           Data.Char        (isSpace)
import           Data.List
import           Data.Maybe
import           Text.Regex.Posix

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace



trimParen :: String -> String
trimParen s = trimParen' 0 s
  where trimParen' :: Int -> String -> String
        trimParen _ [] = []
        trimParen' 1 (')':_) = []
        trimParen' 0 ('(':as) = trimParen' 1 as
        trimParen' 0 (_:as) = trimParen' 0 as
        trimParen' n (a:as) = a : trimParen' n' as
          where n' = case a of
                      '(' -> n+1
                      ')' -> n-1
                      _   -> n

afterTrimParen :: String -> String
afterTrimParen s = afterTrimParen' 0 s
  where afterTrimParen' 1 (')':as) = as
        afterTrimParen' n (a:as) = afterTrimParen' n' as
          where n' = case a of
                       '(' -> n+1
                       ')' -> n-1
                       _   -> n

someFunc :: IO ()
someFunc = do
  l <- getLine
  print $ zadanie3 l



data Statement = Var Char
               | Neg Statement
               | Statement `Alt` Statement
               | Statement `Con` Statement
               | Statement `Imp` Statement
               | Statement `Equ` Statement

infixl 2 `Alt`
infixl 2 `Con`
infixl 1 `Imp`
infixl 1 `Equ`

instance Read Statement where
  readsPrec _ input  | (head $ trim $ trimParen input) `elem` ['a'..'z'] =
                       [(Var (head $ trim $ trimParen input), afterTrimParen input)]
                     | (head $ trim $ trimParen $ trim input) == '~' =
                        case readsPrec 1 $ tail $ trimParen input of
                          []        -> []
                          [(st,rs)] -> [(Neg st, afterTrimParen input)]
                     | otherwise = do
                         let input' = trim $ trimParen input
                         (st1, rs1) <- readsPrec 1 input'
                         f <- case (afterTrimParen input') =~ "[^ \\(\\)]+" of
                                   "v"   -> return Alt
                                   "^"   -> return Con
                                   "->"  -> return Imp
                                   "<=>" -> return Equ
                                   _     -> []
                         (st2, rs2) <- readsPrec 1 $ dropWhile (/='(') $ trim $ afterTrimParen input'
                         return (f st1 st2, rs2)

instance Show Statement where
  show (Var x)   = "(" ++[x] ++")"
  show (Neg x)   = "(~" ++ show x ++ ")"
  show (Alt x y) = "("++ show x ++ " v " ++ show y ++ ")"
  show (Con x y) = "("++ show x ++ " ^ " ++ show y ++ ")"
  show (Imp x y) = "("++ show x ++ " -> " ++ show y ++ ")"
  show (Equ x y) = "("++ show x ++ " <=> " ++ show y ++ ")"



getVariables :: Statement -> [Char]
getVariables (Var x)   = [x]
getVariables (Neg x)   = nub $ getVariables x
getVariables (Alt x y) = nub $ getVariables x ++ getVariables y
getVariables (Con x y) = nub $ getVariables x ++ getVariables y
getVariables (Imp x y) = nub $ getVariables x ++ getVariables y
getVariables (Equ x y) = nub $ getVariables x ++ getVariables y



valuations :: [Char] -> [[(Char,Bool)]]
valuations l = map (zip l) $ sequence $ replicate (length l) [False, True]

eval' :: [(Char,Bool)] -> Statement -> Bool
eval' l (Var x)   = fromJust $ lookup x l
eval' l (Neg x)   = not $ eval' l x
eval' l (Alt x y) = eval' l x || eval' l y
eval' l (Con x y) = eval' l x && eval' l y
eval' l (Imp x y) = (not $ eval' l x) || eval' l y
eval' l (Equ x y) = eval' l x == eval' l y

eval :: Statement -> [([(Char,Bool)],Bool)]
eval s = map (\i -> (i, eval' i s)) val
  where var = getVariables s
        val = valuations var



zadanie1 :: [(Char,Bool)] -> Statement
zadanie1 [(c,b)] =
  if b
  then Neg (Var c)
  else Var c
zadanie1 ((c,b):as) =
  if b
  then Neg (Var c) `Alt` zadanie1 as
  else Var c `Alt` zadanie1 as


zadanie2 :: [Statement] -> Statement
zadanie2 [a]    = a
zadanie2 (a:as) = a `Con` zadanie2 as


zadanie3 :: String -> Statement
zadanie3 s = zadanie2 mapped
  where table = eval $ read s
        filtered = filter (not . snd) table
        mapped = map (zadanie1 . fst) filtered

