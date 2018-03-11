data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving (Show, Eq)


isNumerical :: Term -> Bool
isNumerical TmZero = True
isNumerical (TmSucc t1) = isNumerical t1
isNumerical _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t
  | isNumerical t = True
  | otherwise = False


isTrue :: Term -> Bool
isTrue TmTrue = True
isTrue _ = False

isFalse :: Term -> Bool
isFalse TmFalse = True
isFalse _ = False

isZero :: Term -> Bool
isZero TmZero = True
isZero _ = False


bsEval :: Term -> Term
bsEval TmTrue = TmTrue
bsEval TmFalse = TmFalse
bsEval TmZero = TmZero
bsEval (TmIf t1 t2 t3) = ev (bsEval t1) where
  ev TmTrue = bsEval t2
  ev TmFalse = bsEval t3
bsEval (TmSucc t1) = TmSucc (bsEval t1)
bsEval (TmPred t1) = ev (bsEval t1) where
  ev TmZero = TmZero
  ev (TmSucc t2) = t2
bsEval (TmIsZero t1) = ev (bsEval t1) where
  ev TmZero = TmTrue
  ev (TmSucc t2) = TmFalse



main :: IO()
main = do
  print $ bsEval TmTrue
  print $ "if"
  print $ bsEval (TmIf TmTrue TmFalse TmTrue)
  print $ bsEval (TmIf (TmIf TmFalse TmTrue TmFalse) TmFalse TmTrue)
  print $ "succ/pred"
  print $ bsEval (TmSucc (TmZero))
  print $ bsEval (TmSucc (TmSucc (TmPred TmZero)))
  print $ bsEval (TmPred (TmSucc (TmPred TmZero)))
  print $ bsEval (TmPred (TmPred (TmSucc TmZero)))
  print $ "iszero"
  print $ bsEval (TmIsZero (TmPred (TmSucc TmZero)))
  print $ bsEval (TmIsZero (TmSucc (TmPred TmZero)))
  print $ bsEval (TmIsZero (TmSucc TmZero))
