{
module Drammar where
import Dokens
}

%name calc 
%tokentype { Token } 
%error { happyError }
%token
    given   { TokenGiven _ }
    take    { TokenTake _ }
    such    { TokenSuch _ }
    that    { TokenThat _ }
    and     { TokenAnd _ }
    obey    { TokenObey _ }
    var     { TokenVar _ $$ }
    '='     { TokenEq _ }
    '('     { TokenLPar _ }
    ')'     { TokenRPar _ }
    ','     { TokenComma _ }
    file    { TokenFile _ $$ }

%%

Exp :: { Exp }
Exp : given Query such that Assignment take Output { Given $2 $5 $7 }
     | given Query obey Condition such that Assignment take Output { GivenC $2 $4 $7 $9}

Query : Query and Query { And $1 $3 }
      | var '(' List ')' { Relate $1 $3 }

Condition : var '=' var { Compare $1 $3 }
           | Condition and Condition { AndC $1 $3 }

Assignment : var '=' file { Assign $1 $3 }
           | Assignment and Assignment { AndA $1 $3 }

Output : '(' List ')' { Format $2 }

List : var { Put $1 }
     | var ',' List { Add $1 $3 }

{

data Exp = Given Query Assignment Output |
    GivenC Query Condition Assignment Output
    deriving Show
	
data Query =
    Relate String List    |
    And Query Query
    deriving Show

data Condition =
    Compare String String |
    AndC Condition Condition
    deriving Show
	
data Assignment =
    Assign String String |
    AndA Assignment Assignment
    deriving Show
	
data Output = Format List
    deriving Show
	
data List =
    Put String |
    Add String List
    deriving Show


runCalc :: String -> Exp
runCalc = calc . alexScanTokens

happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
    where
    lcn =   case tks of
          [] -> "end of file"
          tk:_ -> "line " ++ show l ++ ", column " ++ show c
            where
            AlexPn _ l c = token_posn tk
}
