{
module Parser
  ( Parser
  , parseFormula
  , parseStmt
  , parseFile
  , parseLogic
) where

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Error.Class (throwError)

import Data.Char
import Data.Maybe
import Core
import Common
import Modal
import Axioms
}

%name parseFormula FExp
%name parseStmt    Stmt
%name parseFile    File
%name parseLogic   Logic

%tokentype { Token }
%error { parseError }
%lexer { modalLexer } { TEOF }

%monad { Parser } { thenP } { returnP }

%token
  '='       { TEq }
  '('       { TOpenParens }
  ')'       { TCloseParens }
  '{'       { TOpenBraces }
  '}'       { TCloseBraces }
  '['       { TOpenSqBrackets }
  ']'       { TCloseSqBrackets }
  ','       { TSep }
  '/'       { TSlash }
  '->'      { TImply }
  '<->'     { TIff }
  '||-'     { TSequent }
  ident     { TIdent $$ }
  keyword   { TKeyword $$ }
  logic     { TLogic $$ }
  axiom     { TAxiom $$ }
  and       { TAnd }
  or        { TOr }
  not       { TNot }
  bottom    { TBottom }
  top       { TTop }
  square    { TSquare }
  diamond   { TDiamond }
  def       { TDef }
  set       { TSet }
  frame     { TFrame }
  tag       { TTag }
  isValid   { TIsValid }
  isSatis   { TIsSatis }
  assume    { TAssume }

%right '<->'
%right '->'
%left or
%left and
%nonassoc not
%nonassoc square diamond

{- Comentario sobre la asociatividad del "si y solo si":
-- Por lo general, al escribir p <-> q <-> r nos referimos a (p <-> q) && (q <-> r).
-- Darle una asociatividad al operador llevaria al anterior ejemplo a ser parseado
-- como (p <-> q) <-> r o como p <-> (q <-> r), y ninguno de ellos es la idea.
-- Para evitar errores al escribir formulas, no se permitira tal tipo de
-- expresiones, sino que deberan ser escritas en la forma conjuntiva explicitamente.
-}

%%

-- ######## UTILITY PARSERS ########
{- Esta es la forma recomendada por Happy para parsear secuencias.
-- Si bien el resultado esta del reverso (algo que no importa aca)
-- el parser gana en eficiencia y uso de stack. -}
collection(p, sep)  : collection(p, sep) sep p     { $3 : $1 }
                    | p                            { [$1] }
                    |                              { [] }

-- Set :: { [String] }  -- Conjunto matematico por extension, no el token set
Set(p) : '{' collection(p, ',') '}' { $2 }

-- ElementMapping  :: { (String, [String]) }
ElementMapping(dom,codom)  : dom '->' Set(codom)   { ($1, $3) }

-- Map :: { [(String, [String])] }
Map(dom,codom) : '{' collection(ElementMapping(dom,codom), ',') '}' { $2 }

world :: { String }
world : ident   { $1 }
      | keyword { $1 }

-- ######## GRAMMAR PARSERS ########
File    :: { [SStmt] }
File    : Stmt File { $1 : $2 }
        |           { [] }

Stmt  :: { SStmt }
Stmt  : def ident '=' FExp { Def $2 $4 }
      | Exp                { Expr $1 }
      | SetStmt            { Set $1 }

SetStmt :: { SetStmt }
SetStmt : set frame  '=' Map(world, world) { Frame (buildFrame $4) }
        | set tag    '=' Map(world, keyword) { Tag   (buildTag   $4) }

Exp :: { SOp }
Exp : isValid FExp       { Valid $2 }
    | isSatis FExp       { Satis $2 }
    | assume Logic       { Assume $2 }
    | world '||-' FExp { Sequent $1 $3 }

FExp  :: { Scheme Atom }
FExp  : FExp and FExp   { LAnd $1 $3 }
      | FExp or  FExp   { LOr  $1 $3 }
      | FExp '->'  FExp { LImply $1 $3}
      | FExp '<->' FExp { LIff $1 $3 }
      | not FExp        { LNot $2 }
      | square FExp     { LSquare $2 }
      | diamond FExp    { LDiamond $2 }
      | bottom          { LBottom }
      | top             { LTop }
      | keyword         { LAtomic $1 }
      | ident           { LIdent $1 }
      | FExp '[' FExp '/' keyword ']' { LSub $1 $3 $5 }
      | '(' FExp ')'    { $2 }

Logic   :: { SLogic }
        : logic         { LogicIdent $1 }
        | Set(axiom)    { AxiomsList $1 }

{
data Token  = TKeyword String -- Built in function or atomic proposition identifier
            | TIdent   String -- Global definition identifier (used for schemes)
            | TLogic   String -- Logic identifier, written as Logic-ID where ID is the identifier
            | TAxiom   String -- Axiom identifier, written as Axiom-ID where ID is the identifier
            | TDef
            | TEq
            | TSet
            -- Formulas
            | TAnd
            | TOr
            | TNot
            | TImply
            | TIff
            | TBottom
            | TTop
            | TSquare
            | TDiamond
            -- Operadores / BIF
            | TIsValid
            | TIsSatis
            | TSequent
            | TAssume
            -- Modelo
            | TFrame
            | TTag
            -- Sintaxis Concreta
            | TSep
            | TSlash
            | TOpenBraces
            | TOpenParens
            | TCloseBraces
            | TCloseParens
            | TOpenSqBrackets
            | TCloseSqBrackets
            | TEOF
            deriving Show

type Result = Either String

type LineNumber = Int
type Filename = String

type Parser a = String -> LineNumber -> Filename -> Result a

formatError :: LineNumber -> Filename -> String -> String
formatError lineno file msg = foldr1 (++) [file, (':':(show lineno)),
                              ". ", msg]

parseError :: Token -> Parser a
parseError _ s lineno file = Left $ formatError lineno file "Error de parseo"

returnP :: a -> Parser a
returnP x s lineno file = Right x

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP p f = \s lineno file -> case p s lineno file of
                                Left  b -> Left b
                                Right a -> f a s lineno file

modalLexer :: (Token -> Parser a) -> Parser a
modalLexer cont s n path =
  case s of
    [] -> cont TEOF [] n path
    ('\n':r) -> modalLexer cont r (n+1) path
    ('-':('-':r)) -> modalLexer cont (dropWhile ((/=) '\n') r) n path
    ('{':('-':r)) -> consumirBK 0 n path cont r
    ('-':('}':r)) -> Left $ "Línea "++(show n)++": Comentario no abierto"
    ('[':(']':r)) -> cont TSquare  r n path
    ('<':('>':r)) -> cont TDiamond r n path
    ('|':('|':('-':r))) -> cont TSequent r n path
    ('=':r) -> cont TEq  r n path
    (',':r) -> cont TSep r n path
    ('/':r) -> cont TSlash r n path
    ('(':r) -> cont TOpenParens  r n path
    (')':r) -> cont TCloseParens r n path
    ('{':r) -> cont TOpenBraces  r n path
    ('}':r) -> cont TCloseBraces r n path
    ('[':r) -> cont TOpenSqBrackets  r n path
    (']':r) -> cont TCloseSqBrackets r n path
    ('~':r) ->        cont TNot   r n path
    ('&':('&':r))  -> cont TAnd   r n path
    ('|':('|':r))  -> cont TOr    r n path
    ('-':('>':r))  -> cont TImply r n path
    ('<':('-':('>':r))) -> cont TIff r n path
    (c:r) | isAlpha c -> if isUpper c then lexIdent (c:r) else lexKeyword (c:r)
          | isSpace c -> modalLexer cont r n path
    other -> Left $ formatError n path ("Error de lexer: " ++ other)
  where
    consumirBK anidado cl path cont s =
      case s of
        ('-':('-':cs)) -> consumirBK anidado cl path cont $ dropWhile ((/=) '\n') cs
        ('{':('-':cs)) -> consumirBK (anidado+1) cl path cont cs
        ('-':('}':cs)) -> case anidado of
                            0 -> modalLexer cont cs cl path
                            _ -> consumirBK (anidado-1) cl path cont cs
        ('\n':cs) -> consumirBK anidado (cl+1) path cont cs
        (_:cs) -> consumirBK anidado cl path cont cs
    lexIdent cs = case span isAlphaNum cs of
                    ("T",   r) -> cont TTop r n path
                    ("F",   r) -> cont TBottom r n path
                    ("Logic", ('-':r)) -> let (ident, r') = span isAlphaNum r in cont (TLogic ident) r' n path
                    ("Axiom", ('-':r)) -> let (ident, r') = span isAlphaNum r in cont (TAxiom ident) r' n path
                    (ident, r) -> cont (TIdent ident) r n path
    -- Los identificadores comienzan por un caracter alfabetico pero pueden luego contener caracteres numericos
    lexKeyword cs = case span isAlphaNum cs of
                      ("set"  , r) -> cont TSet       r n path
                      ("def"  , r) -> cont TDef       r n path
                      ("and"  , r) -> cont TAnd       r n path
                      ("or"   , r) -> cont TOr        r n path
                      ("not"  , r) -> cont TNot       r n path
                      ("sq"   , r) -> cont TSquare    r n path
                      ("dia"  , r) -> cont TDiamond   r n path
                      ("frame", r) -> cont TFrame     r n path
                      ("tag"  , r) -> cont TTag       r n path
                      ("isValid", r) -> cont TIsValid r n path
                      ("isSatis", r) -> cont TIsSatis r n path
                      ("assume" , r) -> cont TAssume  r n path
                      (kw     , r) -> cont (TKeyword kw) r n path
}
