module Main where

data Term
  = Index Int
  | Abstraction Term
  | Application Term Term
  deriving (Eq)

instance Show Term where
  show (Index name) = show name
  show (Abstraction body) = "\\. " ++ show body
  show (Application fn arg) =
    let argStr = case arg of
          Index _ -> show arg
          _ -> "(" ++ show arg ++ ")"
        fnStr = case fn of
          Index _ -> show fn
          Application _ _ -> show fn
          _ -> "(" ++ show fn ++ ")"
     in fnStr ++ " " ++ argStr

smallStep :: Term -> Maybe Term
smallStep (Application (Abstraction body) arg@(Abstraction _)) =
  Just $ shift (-1) 0 $ sub (0, shift 1 0 arg) body
smallStep (Application fn@(Abstraction _) arg) =
  case smallStep arg of
    Just argStep -> Just $ Application fn argStep
    Nothing -> Nothing
smallStep (Application fn arg) =
  case smallStep fn of
    Just fnStep -> Just $ Application fnStep arg
    Nothing -> Nothing
smallStep _ = Nothing

isNormal :: Term -> Bool
isNormal (Abstraction _) = True
isNormal _ = False

shift :: Int -> Int -> Term -> Term
shift amount cutoff term =
  case term of
    Index ix ->
      if ix >= cutoff
        then Index (ix + amount)
        else Index ix
    Application t1 t2 ->
      Application
        (shift amount cutoff t1)
        (shift amount cutoff t2)
    Abstraction body ->
      Abstraction (shift amount (cutoff + 1) body)

sub :: (Int, Term) -> Term -> Term
sub (valIx, newTerm) term =
  case term of
    Index ix ->
      if ix == valIx
        then newTerm
        else term
    Abstraction body ->
      Abstraction (sub (valIx + 1, shift 1 0 newTerm) body)
    Application t1 t2 ->
      Application (sub (valIx, newTerm) t1) (sub (valIx, newTerm) t2)

printSteps :: Term -> IO ()
printSteps term =
  if isNormal term
    then -- \|| nextTerm == term
    do
      putStrLn $ "= " ++ show term
    else case smallStep term of
      Just nextTerm ->
        if nextTerm == term
          then do putStrLn $ "= " ++ show term ++ " ...repeating"
          else do
            putStrLn $ "> " ++ show term
            printSteps nextTerm
      Nothing ->
        putStrLn $ "= " ++ show term ++ "(no more rules)"

main :: IO ()
main =
  let lid = Abstraction (Index 0)
      lidlid = Application lid lid

      tru = Abstraction (Abstraction (Index 1))
      fls = Abstraction (Abstraction (Index 0))

      land = Abstraction $ Abstraction $ Application (Application (Index 0) (Index 1)) fls
      lor = Abstraction $ Abstraction $ Application (Application (Index 0) tru) (Index 1)
      ltest = Abstraction $ Abstraction $ Abstraction $ Application (Application (Index 2) (Index 1)) (Index 0)
      ortrufls = Application (Application lor tru) fls

      c0 = Abstraction (Abstraction (Index 0))
      c1 = Abstraction (Abstraction (Application (Index 1) (Index 0)))

      plus =
        Abstraction
          ( Abstraction
              ( Abstraction
                  ( Abstraction
                      (Application (Application (Index 3) (Index 1)) (Application (Application (Index 2) (Index 1)) (Index 0)))
                  )
              )
          )

      plus00 = Application (Application plus c0) c0
      plus11 = Application (Application plus c1) c1

      isZero = Abstraction (Application (Application (Index 0) (Abstraction fls)) tru)

      test =
        Application
          (Abstraction (Application (Application (Index 1) (Index 0)) (Index 2)))
          (Abstraction $ Index 0)
   in do
        print lid
        print lidlid
        printSteps lidlid
        putStrLn ""

        putStrLn $ "tru = " ++ show tru
        putStrLn $ "fls = " ++ show fls
        putStrLn $ "or = " ++ show lor
        putStrLn "true & false"
        printSteps (Application (Application land tru) fls)
        putStrLn "true & true"
        printSteps (Application (Application land tru) tru)
        putStrLn "true | false"
        printSteps ortrufls
        -- printSteps (Application (Application (Application ltest ortrufls) tru) fls)
        putStrLn "true | true"
        printSteps (Application (Application lor tru) tru)
        putStrLn ""

        putStrLn "c0"
        print c0
        putStrLn "iszero c0"
        printSteps $ Application isZero c0
        putStrLn "c1"
        print c1
        putStrLn "iszero c1"
        printSteps $ Application isZero c1
        putStrLn ""

        putStrLn "test"
        print test
        printSteps test
        putStrLn ""

        putStr "plus = "
        print plus
        putStrLn "0+0 =? 0 "
        printSteps $ Application isZero plus00
        putStrLn "1+1 =? 0 "
        printSteps $ Application isZero plus11

-- print $ plus11 & smallStep & smallStep & smallStep & smallStep & smallStep
