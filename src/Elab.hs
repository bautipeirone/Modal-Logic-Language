module Elab
  ( elab
  , elabStmt
) where

import Data.Maybe (fromMaybe)

import Common
import Core
import State

undefVarError :: Show a => a -> Either String (Formula a)
undefVarError v = Left $ "Undefined identifier " ++ show v

elab :: DefTable Atom -> Scheme Atom -> Either String (Formula Atom)
elab = elab' []
  where
    elab' :: DefTable Atom -> DefTable Atom -> Scheme Atom -> Either String (Formula Atom)
    elab' env def LBottom        = return Bottom
    elab' env def LTop           = return Top
    elab' env def (LAtomic x)    = return $ sub env (Atomic x)
    elab' env def (LIdent v)     = maybe (undefVarError v) (return . sub env) (lookup v def)
    elab' env def (LAnd s1 s2)   = do f1 <- elab' env def s1
                                      f2 <- elab' env def s2
                                      return $ And f1 f2
    elab' env def (LOr s1 s2)    = do f1 <- elab' env def s1
                                      f2 <- elab' env def s2
                                      return $ Or f1 f2
    elab' env def (LImply s1 s2) = do f1 <- elab' env def s1
                                      f2 <- elab' env def s2
                                      return $ Imply f1 f2
    elab' env def (LIff s1 s2)   = do f1 <- elab' env def s1
                                      f2 <- elab' env def s2
                                      return $ Iff f1 f2
    elab' env def (LNot s)       = do f <- elab' env def s
                                      return $ Not f
    elab' env def (LSquare s)    = do f <- elab' env def s
                                      return $ Square f
    elab' env def (LDiamond s)   = do f <- elab' env def s
                                      return $ Diamond f
    elab' env def (LSub s1 s2 x) = do f2 <- elab' env def s2
                                      elab' ((x,f2):env) def s1
    sub :: DefTable Atom -> Formula Atom -> Formula Atom
    sub env p@(Atomic x)   = fromMaybe p (lookup x env)
    sub env Top            = Top
    sub env Bottom         = Bottom
    sub env (Not      f )  = Not (sub env f)
    sub env (Square   f )  = Square (sub env f)
    sub env (Diamond  f )  = Diamond (sub env f)
    sub env (And   f1 f2)  = And (sub env f1) (sub env f2)
    sub env (Or    f1 f2)  = Or    (sub env f1) (sub env f2)
    sub env (Imply f1 f2)  = Imply (sub env f1) (sub env f2)
    sub env (Iff   f1 f2)  = Iff   (sub env f1) (sub env f2)

elabLogic :: SLogic -> Either String Logic
elabLogic (LogicIdent id ) = identToLogic id
elabLogic (AxiomsList axs) = listToLogic axs

elabOp :: DefTable Atom -> SOp -> Either String Op
elabOp def (Valid sch) = fmap Valid (elab def sch)
elabOp def (Satis sch) = fmap Satis (elab def sch)
elabOp def (Sequent w sch) = fmap (Sequent w) (elab def sch)
elabOp _   (Assume sl) = fmap Assume (elabLogic sl)

liftElab :: Either String a -> RT a
liftElab (Left s) = runtimeError s
liftElab (Right f) = return f

elabStmt :: DefTable Atom -> SStmt -> RT Stmt
-- elabStmt def stmt = liftElab $ fmap (elab def) stmt
elabStmt def (Def a sch) = Def a <$> liftElab (elab def sch)
elabStmt def (Expr op)   = Expr <$> liftElab (elabOp def op)
elabStmt _   (Set s)     = return (Set s)
