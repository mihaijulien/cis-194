module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state varName val = state'
                     where
                       state'::State
                       state' varName'
                         | varName == varName' = val
                         | otherwise = state varName'

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var varName) = state varName
evalE _ (Val value) = value
-- binary arithmetic operators
evalE state (Op expr1 Plus expr2) = evalE state expr1 + evalE state expr2
evalE state (Op expr1 Minus expr2) = evalE state expr1 - evalE state expr2
evalE state (Op expr1 Times expr2) = evalE state expr1 * evalE state expr2
evalE state (Op expr1 Divide expr2) = evalE state expr1 `div` evalE state expr2
-- comparison 
evalE state (Op expr1 Gt expr2) = if (evalE state expr1 > evalE state expr2) == True then 1 else 0
evalE state (Op expr1 Ge expr2) = if (evalE state expr1 >= evalE state expr2) == True then 1 else 0
evalE state (Op expr1 Lt expr2) = if (evalE state expr1 < evalE state expr2) == True then 1 else 0
evalE state (Op expr1 Le expr2) = if (evalE state expr1 <= evalE state expr2) == True then 1 else 0
evalE state (Op expr1 Eql expr2) = if (evalE state expr1 == evalE state expr2) == True then 1 else 0


-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar statement =  case statement of
                       Assign var expr -> DAssign var expr
                       If expr st1 st2-> DIf expr (desugar st1) (desugar st2)
                       While expr st-> DWhile expr (desugar st)
                       Sequence st1 st2 -> DSequence (desugar st1) (desugar st2)
                       Skip -> DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAsign var expr) = extend state var $ evalE state expr 
--TODO rest of DietStatements
evalSimple state DSkip = state

run :: State -> Statement -> State
run state statement = evalSimple state $ desugar statement

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
