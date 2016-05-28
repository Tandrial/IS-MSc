{-# OPTIONS_GHC -Wall -Werror #-}

import Data.Char

-- Ein MiniL-Programm in "intuitiver", C-artiger Syntax
-- ====================================================

{-
  {
    decl x;
    decl n;
    decl z;

    x = 2;
    n = 10;

    z = 1;
    while (n > 0) {
      if (n % 2 == 0) {
        x = x * x;
        n = n / 2;
      } else {
        z = z * x;
        n = n - 1;
      }
    }

    return z;
  }
-}

-- Datentypen zur Definition der Syntax der Quellsprache MiniL
-- ===========================================================

data Expression = Const  String
                | Var    String
                | Unary  String Expression
                | Binary String Expression Expression
                  deriving Show

data Statement = While  Expression Statement
               | Alt    Expression Statement Statement
               | Cond   Expression Statement
               | Assign String     Expression
               | Return Expression
               | Block  [String] [Statement]
                 deriving Show

-- Datentypen zur Definition der Zielsprache
-- =========================================

data BC_ArithExpr = BC_Const Integer
                  | BC_Var   String
                  | BC_Neg   BC_ArithExpr
                  | BC_Add   BC_ArithExpr BC_ArithExpr
                  | BC_Sub   BC_ArithExpr BC_ArithExpr
                  | BC_Mult  BC_ArithExpr BC_ArithExpr
                  | BC_Div   BC_ArithExpr BC_ArithExpr
                  | BC_Mod   BC_ArithExpr BC_ArithExpr
                    deriving Show

data BC_BoolExpr = BC_T
                 | BC_F
                 | BC_Not BC_BoolExpr
                 | BC_And BC_BoolExpr  BC_BoolExpr
                 | BC_Or  BC_BoolExpr  BC_BoolExpr
                 | BC_Eq  BC_ArithExpr BC_ArithExpr
                 | BC_Lt  BC_ArithExpr BC_ArithExpr
--               | BC_Eb  BC_BoolExpr  BC_BoolExpr
                   deriving Show

data BC_Statement = BC_While  BC_BoolExpr BC_Statement
                  | BC_If     BC_BoolExpr BC_Statement BC_Statement
                  | BC_Assign BC_LVar     BC_ArithExpr
                  | BC_Block  [BC_Decl]  [BC_Statement]
                    deriving Show

data BC_Decl = BC_Decl String
               deriving Show

data BC_LVar = BC_LVar String
               deriving Show

-- Compiler von Quell- in Zielsprache
-- ==================================

compile :: Statement -> BC_Statement
compile = mkStatement

mkStatement :: Statement -> BC_Statement
mkStatement (While b s)   = BC_While (mkBoolExpr b) (mkStatement s)
mkStatement (Alt b s1 s2) = BC_If (mkBoolExpr b) (mkStatement s1) (mkStatement s2)
mkStatement (Cond b s)    = BC_If (mkBoolExpr b) (mkStatement s) (BC_Block [] [])
mkStatement (Assign v e)  = BC_Assign (BC_LVar v) (mkExpr e)
mkStatement (Return e)    = BC_Assign (BC_LVar "__result") (mkExpr e)
mkStatement (Block ds ss) = BC_Block (map BC_Decl ds) (map mkStatement ss)

mkBoolExpr :: Expression -> BC_BoolExpr
mkBoolExpr (Const "t")          = BC_T
mkBoolExpr (Const "f")          = BC_F
mkBoolExpr (Unary "not" e)      = BC_Not (mkBoolExpr e)
mkBoolExpr (Binary "and" e1 e2) = BC_And (mkBoolExpr e1) (mkBoolExpr e2)
mkBoolExpr (Binary "or"  e1 e2) = BC_Or  (mkBoolExpr e1) (mkBoolExpr e2)
mkBoolExpr (Binary "=="  e1 e2) = BC_Eq  (mkExpr e1)     (mkExpr e2)
mkBoolExpr (Binary "<"   e1 e2) = BC_Lt  (mkExpr e1)     (mkExpr e2)
mkBoolExpr (Binary ">"   e1 e2) = BC_Lt  (mkExpr e2)     (mkExpr e1)
mkBoolExpr (Binary "<="  e1 e2) = BC_Not (BC_Lt (mkExpr e2) (mkExpr e1))
mkBoolExpr (Binary ">="  e1 e2) = BC_Not (BC_Lt (mkExpr e1) (mkExpr e2))
mkBoolExpr (Binary "!="  e1 e2) = BC_Not (BC_Eq (mkExpr e1) (mkExpr e2))
mkBoolExpr expr                 = error ("Compile Error:" ++ show expr)

mkExpr :: Expression -> BC_ArithExpr
mkExpr (Const n)          = BC_Const (mkInteger n)
mkExpr (Var n)            = BC_Var n
mkExpr (Unary "-" e)      = BC_Neg  (mkExpr e)
mkExpr (Binary "+" e1 e2) = BC_Add  (mkExpr e1) (mkExpr e2)
mkExpr (Binary "-" e1 e2) = BC_Sub  (mkExpr e1) (mkExpr e2)
mkExpr (Binary "*" e1 e2) = BC_Mult (mkExpr e1) (mkExpr e2)
mkExpr (Binary "/" e1 e2) = BC_Div  (mkExpr e1) (mkExpr e2)
mkExpr (Binary "%" e1 e2) = BC_Mod  (mkExpr e1) (mkExpr e2)
mkExpr expr               = error ("Compile Error:" ++ show expr)

mkInteger :: String -> Integer
mkInteger = foldl (\s x -> 10 * s + toInteger (ord x - ord '0')) 0

-- Realisierung des Speichermodells
-- ================================

-- Ein Frame repräsentiert eine Liste von Variablen mit zugehörigen Werten.

data Frame = Frame [(String, Integer)] deriving Show

decl :: String -> [Frame] -> [Frame]
decl var []                  = [Frame [(var, 0)]]
decl var ((Frame curr_f):fs) =  Frame ((var, 0) : curr_f) : fs

assign :: String -> Integer -> [Frame] -> [Frame]
assign var value = map (\(Frame f) -> Frame (map replaceVar f))
  where replaceVar x = if fst x == var then (var, value)
                                       else x

get :: String -> [Frame] -> Integer
get var []                  = error (var ++ " doesn't exist!")
get var ((Frame curr_f):fs) = if null found then get var fs
                                            else snd . head $ found
  where found = filter ((==var) . fst) curr_f

-- Realisierung des Interpreters
-- =============================

-- "Hauptfunktion" der Interpreter-Realisierung.
run :: BC_Statement -> Integer
run = get "__result" . evalStmt (decl ("__result") [])

evalStmt :: [Frame] -> BC_Statement -> [Frame]
evalStmt fs stmt@(BC_While check body)    = if check' then evalStmt fs' stmt
                                                      else fs
  where check' = evalBoolExpr fs check
        fs'    = evalStmt fs body
evalStmt fs (BC_If check if_b else_b)     = if check' then evalStmt fs if_b
                                                      else evalStmt fs else_b
  where check' = evalBoolExpr fs check
evalStmt fs (BC_Assign (BC_LVar var) val) = assign var (evalArithExpr fs val) fs
evalStmt fs (BC_Block decls stmts)        = foldl (evalStmt) fs' stmts
  where fs' = foldl (\f (BC_Decl x) -> decl x f ) fs decls

evalArithExpr :: [Frame] -> BC_ArithExpr -> Integer
evalArithExpr  _ (BC_Const val)    = val
evalArithExpr fs (BC_Var name)     = get name fs
evalArithExpr fs (BC_Neg expr)     = -(evalArithExpr fs expr)
evalArithExpr fs (BC_Add  e1 e2) = evalArithExpr fs e1   +   evalArithExpr fs e2
evalArithExpr fs (BC_Sub  e1 e2) = evalArithExpr fs e1   -   evalArithExpr fs e2
evalArithExpr fs (BC_Mult e1 e2) = evalArithExpr fs e1   *   evalArithExpr fs e2
evalArithExpr fs (BC_Div  e1 e2) = evalArithExpr fs e1 `div` evalArithExpr fs e2
evalArithExpr fs (BC_Mod  e1 e2) = evalArithExpr fs e1 `mod` evalArithExpr fs e2

evalBoolExpr :: [Frame] -> BC_BoolExpr -> Bool
evalBoolExpr  _ BC_T           = True
evalBoolExpr  _ BC_F           = False
evalBoolExpr fs (BC_Not e)     = not (evalBoolExpr fs e)
evalBoolExpr fs (BC_And e1 e2) = evalBoolExpr  fs e1 && evalBoolExpr  fs e2
evalBoolExpr fs (BC_Or  e1 e2) = evalBoolExpr  fs e1 || evalBoolExpr  fs e2
evalBoolExpr fs (BC_Eq  e1 e2) = evalArithExpr fs e1 == evalArithExpr fs e2
evalBoolExpr fs (BC_Lt  e1 e2) = evalArithExpr fs e1 <  evalArithExpr fs e2

-- Beispielprogramm
-- ================

-- Obiges MiniL-Programm als Statement
p :: Statement
p = Block
      ["x", "n", "z"]
      [Assign "x" (Const "2"),
       Assign "n" (Const "10"),
       Assign "z" (Const "1"),
       While
         (Binary ">" (Var "n") (Const "0"))
         (Alt
           (Binary "==" (Binary "%" (Var "n") (Const "2")) (Const "0"))
           (Block
             []
             [Assign "x" (Binary "*" (Var "x") (Var "x")),
              Assign "n" (Binary "/" (Var "n") (Const "2"))]
           )
           (Block
             []
             [Assign "z" (Binary "*" (Var "z") (Var "x")),
              Assign "n" (Binary "-" (Var "n") (Const "1"))]
           )
         ),
       Return (Var "z")
      ]

-- Obiges MiniL-Programm als BC_Statement
p_BC :: BC_Statement
p_BC = BC_Block
         [BC_Decl "x",
          BC_Decl "n",
          BC_Decl "z"]
         [BC_Assign (BC_LVar "x") (BC_Const 2),
          BC_Assign (BC_LVar "n") (BC_Const 10),
          BC_Assign (BC_LVar "z") (BC_Const 1),
          BC_While
            (BC_Lt (BC_Const 0) (BC_Var "n"))
            (BC_If
              (BC_Eq (BC_Mod (BC_Var "n") (BC_Const 2)) (BC_Const 0))
              (BC_Block
                 []
                 [BC_Assign (BC_LVar "x") (BC_Mult (BC_Var "x") (BC_Var "x")),
                  BC_Assign (BC_LVar "n") (BC_Div (BC_Var "n") (BC_Const 2))]
              )
              (BC_Block
                 []
                 [BC_Assign (BC_LVar "z") (BC_Mult (BC_Var "z") (BC_Var "x")),
                  BC_Assign (BC_LVar "n") (BC_Sub (BC_Var "n") (BC_Const 1))]
              )
            ),
          BC_Assign (BC_LVar "__result") (BC_Var "z")
         ]

-- Auswertung des Statement-Terms

result :: Statement -> Integer
result = run . compile
