module Elab
  ( elab
  , elabStmt
) where

import Data.Maybe (fromMaybe)

import Common
import Core
import State

undefVarError :: Show a => a -> RT (Formula a)
undefVarError v = runtimeError $ "Undefined identifier " ++ show v

elab :: Scheme Atom -> RT (Formula Atom)
elab = elab' []
  where
    elab' :: DefTable Atom -> Scheme Atom -> RT (Formula Atom)
    elab' _   LBottom        = return Bottom
    elab' _   LTop           = return Top
    elab' env (LAtomic x)    = return $ sub env (Atomic x)
    elab' env (LIdent v)     = do def <- snd <$> getEnv
                                  maybe (undefVarError v) (return . sub env) (lookup v def)
    elab' env (LAnd s1 s2)   = do f1 <- elab' env s1
                                  f2 <- elab' env s2
                                  return $ And f1 f2
    elab' env (LOr s1 s2)    = do f1 <- elab' env s1
                                  f2 <- elab' env s2
                                  return $ Or f1 f2
    elab' env (LImply s1 s2) = do f1 <- elab' env s1
                                  f2 <- elab' env s2
                                  return $ Imply f1 f2
    elab' env (LIff s1 s2)   = do f1 <- elab' env s1
                                  f2 <- elab' env s2
                                  return $ Iff f1 f2
    elab' env (LNot s)       = do f <- elab' env s
                                  return $ Not f
    elab' env (LSquare s)    = do f <- elab' env s
                                  return $ Square f
    elab' env (LDiamond s)   = do f <- elab' env s
                                  return $ Diamond f
    elab' env (LSub s1 s2 x) = do f2 <- elab' env s2
                                  elab' ((x,f2):env) s1

    sub :: DefTable Atom -> Formula Atom -> Formula Atom
    sub env p@(Atomic x)   = fromMaybe p (lookup x env)
    sub _   Top            = Top
    sub _   Bottom         = Bottom
    sub env (Not      f )  = Not (sub env f)
    sub env (Square   f )  = Square (sub env f)
    sub env (Diamond  f )  = Diamond (sub env f)
    sub env (And   f1 f2)  = And (sub env f1) (sub env f2)
    sub env (Or    f1 f2)  = Or    (sub env f1) (sub env f2)
    sub env (Imply f1 f2)  = Imply (sub env f1) (sub env f2)
    sub env (Iff   f1 f2)  = Iff   (sub env f1) (sub env f2)

elabLogic :: SLogic -> RT Logic
elabLogic (LogicIdent lid) = liftElab $ identToLogic lid
elabLogic (AxiomsList axs) = liftElab $ listToLogic axs

liftElab :: Either String a -> RT a
liftElab (Left s)  = runtimeError s
liftElab (Right a) = return a

elabOp :: SOp -> RT Op
elabOp (Valid sch)     = fmap Valid (elab sch)
elabOp (Satis sch)     = fmap Satis (elab sch)
elabOp (Sequent w sch) = fmap (Sequent w) (elab sch)
elabOp (Assume sl)     = fmap Assume (elabLogic sl)

elabStmt :: SStmt -> RT Stmt
elabStmt (Def a sch) = Def a <$> elab sch
elabStmt (Expr op)   = Expr  <$> elabOp op
elabStmt (Set s)     = return (Set s)
