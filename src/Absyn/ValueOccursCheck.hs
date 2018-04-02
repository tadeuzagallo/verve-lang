module Absyn.ValueOccursCheck where

import Absyn.Base
import Absyn.Meta
import Typing.State
import Typing.TypeError

import Control.Monad (mapM_, when)

class ValueOccursCheck b where
  valueOccursCheck :: Name -> b Name c d -> Tc Bool

instance ValueOccursCheck BaseExpr where
  -- TRIVIAL
  valueOccursCheck var expr = do
    valueOccursCheckExpr var expr
    return False

valueOccursCheckExpr :: Name -> BaseExpr Name a b -> Tc ()
valueOccursCheckExpr _ VoidExpr = return ()
valueOccursCheckExpr _ (Literal _) = return ()
valueOccursCheckExpr _ (FnExpr _) = return ()

valueOccursCheckExpr var (ParenthesizedExpr x) =
  valueOccursCheckExpr var x

valueOccursCheckExpr var (Ident names _) =
  check var (last names)

valueOccursCheckExpr var (Match { expr, cases }) = do
  valueOccursCheckExpr var expr
  mapM_ (valueOccursCheckCase var) cases

valueOccursCheckExpr var (If { ifCond, ifBody, ifElseBody }) = do
  valueOccursCheckExpr var ifCond
  _ <- mapValueOccursCheck var ifBody
  _ <- mapValueOccursCheck var ifElseBody
  return ()

valueOccursCheckExpr var (Call { callee, args }) = do
  valueOccursCheckExpr var callee
  _ <- mapValueOccursCheck var args
  return ()

valueOccursCheckExpr var (BinOp { lhs, op, rhs }) = do
  check op var
  valueOccursCheckExpr var lhs
  valueOccursCheckExpr var rhs

valueOccursCheckExpr var (Record x) = do
  _ <- mapValueOccursCheck var $ map snd x
  return ()

valueOccursCheckExpr var (List _ items) = do
  _ <- mapValueOccursCheck var items
  return ()

valueOccursCheckExpr var (FieldAccess obj _ _) =
  valueOccursCheckExpr var obj

valueOccursCheckExpr var (TypeCall callee _) =
  valueOccursCheckExpr var callee

valueOccursCheckExpr var (Negate _ expr) =
  valueOccursCheckExpr var expr

instance ValueOccursCheck BaseStmt where
  valueOccursCheck var (Decl x) =
    valueOccursCheck var x

  valueOccursCheck var (Expr x) =
    valueOccursCheck var x

instance ValueOccursCheck BaseDecl where
  valueOccursCheck var (FnStmt (Function { name })) =
    return (var == name)

  valueOccursCheck var (Enum name _ _ ) =
    return (var == name)

  valueOccursCheck var (Class { className }) =
    return (var == className)

  valueOccursCheck var (Operator { opName }) =
    return (var == opName)

  valueOccursCheck var (Interface { intfName }) =
    return (var == intfName)

  valueOccursCheck var (Let (name, _) expr) =
    if var == name
       then return True
       else valueOccursCheck var expr

  valueOccursCheck _ (Implementation {}) =
    return False

  valueOccursCheck _ (TypeAlias {}) =
    return False

valueOccursCheckCase :: Name -> BaseCase Name b c -> Tc ()
valueOccursCheckCase var (Case { pattern, caseBody }) = do
  shadowed <- valueOccursCheckPattern var pattern
  if not shadowed
     then mapValueOccursCheck var caseBody >> return ()
     else return ()

valueOccursCheckPattern :: Name -> BasePattern Name -> Tc Bool
valueOccursCheckPattern _ PatDefault = return False
valueOccursCheckPattern _ (PatLiteral _) = return False

valueOccursCheckPattern var (PatVar x) =
  return (var == x)

valueOccursCheckPattern var (PatRecord x) =
  or <$> mapM (valueOccursCheckPattern var . snd) x

valueOccursCheckPattern var (PatList items rest) = do
  sItems <- or <$> mapM (valueOccursCheckPattern var) items
  sRest <- valueOccursCheckPatternRest var rest
  return (sItems || sRest)

valueOccursCheckPattern var (PatCtor _ args) =
  or <$> mapM (valueOccursCheckPattern var) args

valueOccursCheckPatternRest :: Name -> BasePatternRest Name -> Tc Bool
valueOccursCheckPatternRest _ NoRest = return False
valueOccursCheckPatternRest _ DiscardRest = return False

valueOccursCheckPatternRest var (NamedRest rest) =
  return (var == rest)

mapValueOccursCheck :: ValueOccursCheck e =>  Name -> [e Name a b] -> Tc Bool
mapValueOccursCheck _ [] =
  return False

mapValueOccursCheck var (x : xs) = do
  shadowed <- valueOccursCheck var x
  if shadowed
     then return True
     else mapValueOccursCheck var xs

check :: Name -> Name -> Tc ()
check v1 v2 =
  when (v1 == v2) $ throwError (VariableUsedDuringInitialization v1)
