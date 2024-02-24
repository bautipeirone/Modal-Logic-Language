{
module Parser where

import Core (Result)
import Data.Char
import Core
import Modal
}

%name parseFormula FExp
%name parseStmt    Stmt
%name parseFile    File

%tokentype { Token }
%error { parseError }
%lexer { modalLexer } { TEOF }

%monad { P } { thenP } { returnP }

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

%nonassoc '<->'
%right '->'
%right and
%right or
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

Set :: { [String] }  -- Conjunto matematico por extension, no el token set
Set : '{' collection(keyword, ',') '}' { $2 }

ElementMapping  :: { (String, [String]) }
ElementMapping  : keyword '->' Set     { ($1, $3) }

Map :: { [(String, [String])] }
Map : '{' collection(ElementMapping, ',') '}' { $2 }

-- ######## GRAMMAR PARSERS ########
File    :: { [Stmt String String] }
File    : Stmt File { $1 : $2 }
        |           { [] }

Stmt    :: { Stmt String String }
Stmt    : def ident '=' FExp { Def $2 $4 }
        | SetStmt            { Set $1 }
        | Exp                { Expr $1 }

SetStmt :: { SetStmt String String }
SetStmt : set frame  '=' Map { Frame (buildFrame $4) }
        | set tag    '=' Map { Tag   (buildTag   $4) }

Exp :: { Op String String }
Exp : isValid FExp   { Valid $2 }
    | isSatis FExp   { Satis $2 }
    | keyword '||-' FExp { Sequent $1 $3 }

FExp    :: { Formula String }
FExp    : FExp and FExp   { And $1 $3 }
        | FExp or  FExp   { Or  $1 $3 }
        | not FExp        { Not $2 }
        | FExp '->'  FExp { Imply $1 $3}
        | FExp '<->' FExp { Iff $1 $3 }
        | square FExp     { Square $2 }
        | diamond FExp    { Diamond $2 }
        | bottom          { Bottom }
        | top             { Top }
        | keyword         { Atomic $1 }
        | ident           { Global $1 }
        -- Para poder hacer substituciones que tengan en cuenta
        -- el ambiente no va a ser posible realizar substituciones en tiempo
        -- de parsing. Lo voy a postergar a momento de evaluacion.
        | FExp '[' FExp '/' keyword ']' { Sub $1 $3 $5 }
        | '(' FExp ')'    { $2 }

{
data Token  = TKeyword String -- Built in function or atomic proposition identifier
            | TIdent   String -- Global definition identifier (used for schemes)
            | TDef
            | TEq
            | TUse
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

type LineNumber = Int
type Filename = String

type P a = String -> LineNumber -> Filename -> Result a

formatError :: LineNumber -> Filename -> String -> String
formatError lineno file msg = foldr1 (++) ["[ERROR] ", file, (':':(show lineno)),
                              ". ", msg]

parseError :: Token -> P a
parseError _ s lineno file = Left $ formatError lineno file "Error de parseo"

returnP :: a -> P a
returnP x s lineno file = Right x

thenP :: P a -> (a -> P b) -> P b
thenP p f = \s lineno file -> case p s lineno file of
                                Left  b -> Left b
                                Right a -> f a s lineno file

modalLexer :: (Token -> P a) -> P a
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
    ('!':r) ->       cont TNot   r n path
    ('&':('&':r)) -> cont TAnd   r n path
    ('|':('|':r)) -> cont TOr    r n path
    ('-':('>':r)) -> cont TImply r n path
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
    lexIdent cs = let (ident, r) = span isAlphaNum cs in cont (TIdent ident) r n path
    -- Los identificadores comienzan por un caracter alfabetico pero pueden luego contener caracteres numericos
    lexKeyword cs = case span isAlphaNum cs of
                      --("use"  , r) -> cont TUse    r n path
                      ("set"  , r) -> cont TSet       r n path
                      ("def"  , r) -> cont TDef       r n path
                      ("and"  , r) -> cont TAnd       r n path
                      ("or"   , r) -> cont TOr        r n path
                      ("not"  , r) -> cont TNot       r n path
                      ("F"    , r) -> cont TBottom    r n path
                      ("T"    , r) -> cont TTop       r n path
                      ("sq"   , r) -> cont TSquare    r n path
                      ("dia"  , r) -> cont TDiamond   r n path
                      ("frame", r) -> cont TFrame     r n path
                      ("tag"  , r) -> cont TTag       r n path
                      ("isValid", r) -> cont TIsValid r n path
                      ("isSatis", r) -> cont TIsSatis r n path
                      (kw     , r) -> cont (TKeyword kw) r n path
}