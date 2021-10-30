{-
 - Author: Ian Orzel, iorzel2019@My.fit.edu
 - Course: CSE 4250, Fall 2021
 - Project: Proj4, Tautology Checker
 - Language implementation: Glorious Glasgow Haskell Compilation System, version 8.4.3
 -}

import Data.List


data Stack a = EmptyStack | StackElement a (Stack a)

stackPush :: Stack a -> a -> Stack a
stackPush s x = StackElement x s

stackPeek :: Stack a -> a
stackPeek EmptyStack = error "Attempt to look at top of empty stack!"
stackPeek (StackElement x _) = x

stackPop :: Stack a -> Stack a
stackPop EmptyStack = EmptyStack
stackPop (StackElement _ s) = s


data ListComp a = ListComp [a] [a]

addToFirstList :: (Eq a) => a -> ListComp a -> ListComp a
addToFirstList x (ListComp l1 l2)
    | elem x l1 = ListComp l1 l2
    | otherwise = ListComp (x:l1) l2

addToSecondList :: (Eq a) => a -> ListComp a -> ListComp a
addToSecondList x (ListComp l1 l2)
    | elem x l2 = ListComp l1 l2
    | otherwise = ListComp l1 (x:l2)

combineListComps :: (Eq a ) => ListComp a -> ListComp a -> ListComp a
combineListComps (ListComp l1 l2) (ListComp l3 l4) = ListComp (union l1 l3) (union l2 l4)

checkDoubles :: (Eq a) => ListComp a -> Bool
checkDoubles (ListComp l1 l2) = not $ null (intersect l1 l2)


data Expression = Atom Char | Not Expression | And Expression Expression | Or Expression Expression

instance Show Expression where
    show (Atom c) = [c]
    show (Not e) = "~" ++ (show e)
    show (And e1 e2) = "(" ++ (show e1) ++ " & " ++ (show e2) ++ ")"
    show (Or e1 e2) = "(" ++ (show e1) ++ " v " ++ (show e2) ++ ")"

instance Eq Expression where
    (==) (Atom c1) (Atom c2) = c1 == c2
    (==) (Not e1) (Not e2) = e1 == e2
    (==) (And e1 e2) (And e3 e4) = (e1 == e3) && (e2 == e4)
    (==) (Or e1 e2) (Or e3 e4) = (e1 == e3) && (e2 == e4)
    (==) _ _ = False
    (/=) e1 e2 = not (e1 == e2)

generateExpression :: String -> Expression
generateExpression str = stackPeek $ generateSubExpression EmptyStack str

generateSubExpression :: Stack Expression -> String -> Stack Expression
generateSubExpression s [] = s
generateSubExpression s (c:cs)
    | c == 'A'  = f (binaryOp (Or y x))
    | c == 'C'  = f (binaryOp (Or (Not y) x))
    | c == 'D'  = f (binaryOp (Not (And y x)))
    | c == 'E'  = f (binaryOp (And (Or (Not y) x) (Or y (Not x))))
    | c == 'J'  = f (binaryOp (Or (And (Not y) x) (And y (Not x))))
    | c == 'K'  = f (binaryOp (And y x))
    | c == 'N'  = f (unaryOp (Not x))
    | otherwise = f (stackPush s (Atom c))
    where x = stackPeek s
          y = (stackPeek . stackPop) s
          binaryOp = (stackPush . stackPop . stackPop) s
          unaryOp = (stackPush . stackPop) s
          f = flip generateSubExpression cs

genNNF :: Expression -> Expression
genNNF (Not e) = distributeNot e
genNNF (And e1 e2) = And (genNNF e1) (genNNF e2)
genNNF (Or e1 e2) = Or (genNNF e1) (genNNF e2)
genNNF e = e

distributeNot :: Expression -> Expression
distributeNot (Not e) = genNNF e
distributeNot (And e1 e2) = Or (distributeNot e1) (distributeNot e2)
distributeNot (Or e1 e2) = And (distributeNot e1) (distributeNot e2)
distributeNot e = Not e

genCNF :: Expression -> Expression
genCNF (Or (And e1 e2 ) e3) = genCNF (And (Or e1 e3) (Or e2 e3))
genCNF (Or e1 (And e2 e3)) = genCNF (And (Or e1 e2) (Or e1 e3))
genCNF (Or e1 e2) = Or (genCNF e1) (genCNF e2)
genCNF (And e1 e2) = And (genCNF e1) (genCNF e2)
genCNF (Not e) = Not (genCNF e)
genCNF e = e

genCNFRepeat :: Expression -> Expression
genCNFRepeat e
    | e' /= e =   genCNFRepeat e'
    | otherwise = e
    where e' = genCNF e

checkTautology :: Expression -> Bool
checkTautology (And e1 e2) = (checkTautology e1) && (checkTautology e2)
checkTautology e = checkDoubles $ checkClause e

emptyListComp :: ListComp Char
emptyListComp = ListComp [] []

checkClause :: Expression -> ListComp Char
checkClause (Or e1 e2) = combineListComps (checkClause e1) (checkClause e2)
checkClause (Not (Atom c)) = addToSecondList c emptyListComp
checkClause (Atom c) = addToFirstList c emptyListComp
checkClause _ = error "The expression was not in CNF!"

generateFinalForm :: String -> Expression
generateFinalForm = genCNFRepeat . genNNF . generateExpression


main :: IO ()
main = do
    line <- getLine
    if null line
        then return ()
    else do
        let expr = generateFinalForm $ line
        do
            putStrLn $ (show (checkTautology expr)) ++ " " ++ (show expr)
            main
