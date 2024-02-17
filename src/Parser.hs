{-# OPTIONS_GHC -w #-}
module Parser where

import Data.Char
import Modal
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn6 ([Stmt String])
	| HappyAbsSyn7 (Stmt String)
	| HappyAbsSyn9 (Formula String)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40 :: () => Prelude.Int -> ({-HappyReduction (P) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19 :: () => ({-HappyReduction (P) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,85) ([17408,242,37120,252,9280,63,51472,15,62020,3,0,0,216,4096,969,0,0,37120,60,0,0,0,0,62020,0,15505,0,4,0,8192,0,0,0,0,55296,0,51472,3,62020,0,15505,16384,3876,2048,0,512,0,0,0,0,0,0,0,3464,0,0,0,0,4096,969,4096,0,0,0,0,0,1024,0,384,0,0,4,216,32768,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseFormula","%start_parseStmt","%start_parseFile","File","Stmt","SetStmt","FExp","'='","'('","')'","'{'","'}'","var","and","or","not","imply","iff","bottom","top","sq","dia","def","set","worlds","trans","tag","%eof"]
        bit_start = st Prelude.* 30
        bit_end = (st Prelude.+ 1) Prelude.* 30
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..29]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (11) = happyShift action_7
action_0 (15) = happyShift action_8
action_0 (18) = happyShift action_9
action_0 (21) = happyShift action_10
action_0 (22) = happyShift action_11
action_0 (23) = happyShift action_12
action_0 (24) = happyShift action_13
action_0 (9) = happyGoto action_18
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (11) = happyShift action_7
action_1 (15) = happyShift action_8
action_1 (18) = happyShift action_9
action_1 (21) = happyShift action_10
action_1 (22) = happyShift action_11
action_1 (23) = happyShift action_12
action_1 (24) = happyShift action_13
action_1 (25) = happyShift action_14
action_1 (26) = happyShift action_15
action_1 (7) = happyGoto action_17
action_1 (8) = happyGoto action_5
action_1 (9) = happyGoto action_6
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (11) = happyShift action_7
action_2 (15) = happyShift action_8
action_2 (18) = happyShift action_9
action_2 (21) = happyShift action_10
action_2 (22) = happyShift action_11
action_2 (23) = happyShift action_12
action_2 (24) = happyShift action_13
action_2 (25) = happyShift action_14
action_2 (26) = happyShift action_15
action_2 (6) = happyGoto action_16
action_2 (7) = happyGoto action_4
action_2 (8) = happyGoto action_5
action_2 (9) = happyGoto action_6
action_2 _ = happyReduce_4

action_3 (11) = happyShift action_7
action_3 (15) = happyShift action_8
action_3 (18) = happyShift action_9
action_3 (21) = happyShift action_10
action_3 (22) = happyShift action_11
action_3 (23) = happyShift action_12
action_3 (24) = happyShift action_13
action_3 (25) = happyShift action_14
action_3 (26) = happyShift action_15
action_3 (7) = happyGoto action_4
action_3 (8) = happyGoto action_5
action_3 (9) = happyGoto action_6
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (11) = happyShift action_7
action_4 (15) = happyShift action_8
action_4 (18) = happyShift action_9
action_4 (21) = happyShift action_10
action_4 (22) = happyShift action_11
action_4 (23) = happyShift action_12
action_4 (24) = happyShift action_13
action_4 (25) = happyShift action_14
action_4 (26) = happyShift action_15
action_4 (6) = happyGoto action_29
action_4 (7) = happyGoto action_4
action_4 (8) = happyGoto action_5
action_4 (9) = happyGoto action_6
action_4 _ = happyReduce_4

action_5 _ = happyReduce_6

action_6 (16) = happyShift action_19
action_6 (17) = happyShift action_20
action_6 (19) = happyShift action_21
action_6 (20) = happyShift action_22
action_6 _ = happyReduce_7

action_7 (11) = happyShift action_7
action_7 (15) = happyShift action_8
action_7 (18) = happyShift action_9
action_7 (21) = happyShift action_10
action_7 (22) = happyShift action_11
action_7 (23) = happyShift action_12
action_7 (24) = happyShift action_13
action_7 (9) = happyGoto action_28
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_18

action_9 (11) = happyShift action_7
action_9 (15) = happyShift action_8
action_9 (18) = happyShift action_9
action_9 (21) = happyShift action_10
action_9 (22) = happyShift action_11
action_9 (23) = happyShift action_12
action_9 (24) = happyShift action_13
action_9 (9) = happyGoto action_27
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_16

action_11 _ = happyReduce_17

action_12 (11) = happyShift action_7
action_12 (15) = happyShift action_8
action_12 (18) = happyShift action_9
action_12 (21) = happyShift action_10
action_12 (22) = happyShift action_11
action_12 (23) = happyShift action_12
action_12 (24) = happyShift action_13
action_12 (9) = happyGoto action_26
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (11) = happyShift action_7
action_13 (15) = happyShift action_8
action_13 (18) = happyShift action_9
action_13 (21) = happyShift action_10
action_13 (22) = happyShift action_11
action_13 (23) = happyShift action_12
action_13 (24) = happyShift action_13
action_13 (9) = happyGoto action_25
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (15) = happyShift action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (28) = happyShift action_23
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (30) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (30) = happyAccept
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (16) = happyShift action_19
action_18 (17) = happyShift action_20
action_18 (19) = happyShift action_21
action_18 (20) = happyShift action_22
action_18 (30) = happyAccept
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (11) = happyShift action_7
action_19 (15) = happyShift action_8
action_19 (18) = happyShift action_9
action_19 (21) = happyShift action_10
action_19 (22) = happyShift action_11
action_19 (23) = happyShift action_12
action_19 (24) = happyShift action_13
action_19 (9) = happyGoto action_36
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (11) = happyShift action_7
action_20 (15) = happyShift action_8
action_20 (18) = happyShift action_9
action_20 (21) = happyShift action_10
action_20 (22) = happyShift action_11
action_20 (23) = happyShift action_12
action_20 (24) = happyShift action_13
action_20 (9) = happyGoto action_35
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (11) = happyShift action_7
action_21 (15) = happyShift action_8
action_21 (18) = happyShift action_9
action_21 (21) = happyShift action_10
action_21 (22) = happyShift action_11
action_21 (23) = happyShift action_12
action_21 (24) = happyShift action_13
action_21 (9) = happyGoto action_34
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (11) = happyShift action_7
action_22 (15) = happyShift action_8
action_22 (18) = happyShift action_9
action_22 (21) = happyShift action_10
action_22 (22) = happyShift action_11
action_22 (23) = happyShift action_12
action_22 (24) = happyShift action_13
action_22 (9) = happyGoto action_33
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (10) = happyShift action_32
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (10) = happyShift action_31
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (16) = happyShift action_19
action_25 (17) = happyShift action_20
action_25 (19) = happyShift action_21
action_25 (20) = happyShift action_22
action_25 _ = happyReduce_15

action_26 (16) = happyShift action_19
action_26 (17) = happyShift action_20
action_26 (19) = happyShift action_21
action_26 (20) = happyShift action_22
action_26 _ = happyReduce_14

action_27 (19) = happyShift action_21
action_27 (20) = happyShift action_22
action_27 _ = happyReduce_11

action_28 (12) = happyShift action_30
action_28 (16) = happyShift action_19
action_28 (17) = happyShift action_20
action_28 (19) = happyShift action_21
action_28 (20) = happyShift action_22
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_3

action_30 _ = happyReduce_19

action_31 (11) = happyShift action_7
action_31 (15) = happyShift action_8
action_31 (18) = happyShift action_9
action_31 (21) = happyShift action_10
action_31 (22) = happyShift action_11
action_31 (23) = happyShift action_12
action_31 (24) = happyShift action_13
action_31 (9) = happyGoto action_38
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (13) = happyShift action_37
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (16) = happyShift action_19
action_33 (17) = happyShift action_20
action_33 (19) = happyShift action_21
action_33 (20) = happyShift action_22
action_33 _ = happyReduce_13

action_34 (16) = happyShift action_19
action_34 (17) = happyShift action_20
action_34 (19) = happyShift action_21
action_34 (20) = happyShift action_22
action_34 _ = happyReduce_12

action_35 (17) = happyShift action_20
action_35 (19) = happyShift action_21
action_35 (20) = happyShift action_22
action_35 _ = happyReduce_10

action_36 (16) = happyShift action_19
action_36 (17) = happyShift action_20
action_36 (19) = happyShift action_21
action_36 (20) = happyShift action_22
action_36 _ = happyReduce_9

action_37 (29) = happyShift action_39
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (16) = happyShift action_19
action_38 (17) = happyShift action_20
action_38 (19) = happyShift action_21
action_38 (20) = happyShift action_22
action_38 _ = happyReduce_5

action_39 (14) = happyShift action_40
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_8

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  6 happyReduction_4
happyReduction_4  =  HappyAbsSyn6
		 ([]
	)

happyReduce_5 = happyReduce 4 7 happyReduction_5
happyReduction_5 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Def happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (Expr happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 8 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Set "tag"
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  9 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (And happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Or  happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  9 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Not happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Imply happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Iff happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  9 happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Square happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  9 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Diamond happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn9
		 (Bottom
	)

happyReduce_17 = happySpecReduce_1  9 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn9
		 (Top
	)

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn9
		 (Atomic happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= modalLexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 30 30 tk (HappyState action) sts stk;
	TEq -> cont 10;
	TOpenParens -> cont 11;
	TCloseParens -> cont 12;
	TOpenBraces -> cont 13;
	TCloseBraces -> cont 14;
	TVar happy_dollar_dollar -> cont 15;
	TAnd -> cont 16;
	TOr -> cont 17;
	TNot -> cont 18;
	TImply -> cont 19;
	TIff -> cont 20;
	TBottom -> cont 21;
	TTop -> cont 22;
	TSquare -> cont 23;
	TDiamond -> cont 24;
	TDef -> cont 25;
	TSet -> cont 26;
	TWorlds -> cont 27;
	TTrans -> cont 28;
	TTag -> cont 29;
	_ -> happyError' (tk, [])
	})

happyError_ explist 30 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 :: () => P a -> (a -> P b) -> P b
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> P a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
parseFormula = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

parseStmt = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

parseFile = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Token  = TVar String
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
            -- Modelo
            | TWorlds
            | TTrans
            | TTag
            -- Sintaxis Concreta
            | TOpenBraces
            | TOpenParens
            | TCloseBraces
            | TCloseParens
            | TEOF
            deriving Show

type Result a = Either String a
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
    ('-':('}':cs)) -> Left $ "Línea "++(show n)++": Comentario no abierto"
    ('=':r) -> cont TEq r n path
    ('(':r) -> cont TOpenParens  r n path
    (')':r) -> cont TCloseParens r n path
    ('{':r) -> cont TOpenBraces  r n path
    ('}':r) -> cont TCloseBraces r n path
    (c:r) | isAlpha c -> lexIdent (c:r)
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
    lexIdent ident = case span isAlpha ident of
                        --("use"  , r) -> cont TUse    r n path
                        ("set"  , r) -> cont TSet    r n path
                        ("def"  , r) -> cont TDef    r n path
                        ("and"  , r) -> cont TAnd    r n path
                        ("or"   , r) -> cont TOr     r n path
                        ("not"  , r) -> cont TNot    r n path
                        ("bot"  , r) -> cont TBottom r n path
                        ("top"  , r) -> cont TTop    r n path
                        ("imply", r) -> cont TImply  r n path
                        ("iff"  , r) -> cont TIff    r n path
                        ("sq"   , r) -> cont TSquare r n path
                        ("dia"  , r) -> cont TDiamond r n path
                        ("worlds",r) -> cont TWorlds r n path
                        ("trans", r) -> cont TTrans  r n path
                        ("tag"  , r) -> cont TTag    r n path
                        (var    , r) -> cont (TVar var) r n path
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
