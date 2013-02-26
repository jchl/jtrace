{
module GmlTokens (Token(..), gmlLex) where
}

%wrapper "posn"

$letter = [a-zA-Z]
$digit = 0-9

@operator =
   acos
   |addi
   |addf
   |apply
   |asin
   |clampf
   |cone
   |cos
   |cube
   |cylinder
   |difference
   |divi
   |divf
   |eqi
   |eqf
   |floor
   |frac
   |get
   |getx
   |gety
   |getz
   |if
   |intersect
   |length
   |lessi
   |lessf
   |light
   |modi
   |muli
   |mulf
   |negi
   |negf
   |plane
   |point
   |pointlight
   |real
   |render
   |rotatex
   |rotatey
   |rotatez
   |scale
   |sin
   |sphere
   |spotlight
   |sqrt
   |subi
   |subf
   |translate
   |union
   |uscale

@boolean =
   true
   |false

@num = $digit+
@exp = [eE] \-? @num

tokens :-

   [\ \t\r\n\v]+                         ;
   \% .*                                 ;
   \[                                    { \p -> \s -> TokenOpenBracket }
   \]                                    { \p -> \s -> TokenCloseBracket }
   \{                                    { \p -> \s -> TokenOpenBrace }
   \}                                    { \p -> \s -> TokenCloseBrace }

   @operator                             { \p -> \s -> TokenOperator s }
   true                                  { \p -> \s -> TokenBoolean True }
   false                                 { \p -> \s -> TokenBoolean False }
   $letter [$letter $digit \- _]*        { \p -> \s -> TokenIdentifier s }

   \/ (@operator | @boolean)             { tokenError (\s -> "Cannot rebind " ++ s) }
   \/ $letter [$letter $digit \- _]*     { \p -> \s -> TokenBinder (tail s) }

   \-? @num                              { \p -> \s -> TokenInteger (read s) }
   \-? @num \. @num @exp?                { \p -> \s -> TokenReal (read s) }
   \-? @num @exp                         { \p -> \s -> TokenReal (read s) }
   \" [^\"]* \"                          { \p -> \s -> TokenString (init (tail s)) }

{

tokenError :: (String -> String) -> AlexPosn -> String -> a
tokenError f (AlexPn char line col) s =
   error $ f s ++ " at line " ++ show line ++ ", column " ++ show col

data Token = TokenOpenBracket
           | TokenCloseBracket
           | TokenOpenBrace
           | TokenCloseBrace
           | TokenOperator String
           | TokenIdentifier String
           | TokenBinder String
           | TokenBoolean Bool
           | TokenInteger Int
           | TokenReal Double
           | TokenString String
           deriving (Show)

gmlLex = alexScanTokens

}
