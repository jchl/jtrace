{
module Gml (parse) where

import GmlTokens
import Types
import Operators
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
   '['        { TokenOpenBracket }
   ']'        { TokenCloseBracket }
   '{'        { TokenOpenBrace }
   '}'        { TokenCloseBrace }
   apply      { TokenOperator "apply" }
   if         { TokenOperator "if" }
   render     { TokenOperator "render" }
   operator   { TokenOperator $$ }
   identifier { TokenIdentifier $$ }
   binder     { TokenBinder $$ }
   boolean    { TokenBoolean $$ }
   integer    { TokenInteger $$ }
   real       { TokenReal $$ }
   string     { TokenString $$ }

%%

Program     : TokenList             { reverse $1 }

TokenList   : {- empty -}           { [] }
            | TokenList TokenGroup  { $2 : $1 }

TokenGroup  : Token                 { $1 }
            | '{' TokenList '}'     { PushClosure (reverse $2) }
            | '[' TokenList ']'     { PushArray (reverse $2) }

Token       : apply                 { Apply }
            | if                    { If }
            | render                { Render }
            | operator              { Invoke $1 (operator $1) }
            | identifier            { Lookup $1 }
            | binder                { Bind $1 }
            | boolean               { PushConstant (VBool $1) }
            | integer               { PushConstant (VInt $1) }
            | real                  { PushConstant (VReal $1) }
            | string                { PushConstant (VString $1) }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
