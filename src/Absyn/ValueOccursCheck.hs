module Absyn.ValueOccursCheck where

import Absyn.Base
import Absyn.Meta
import Absyn.Untyped

import Typing.Env
import Typing.TypeError
import Util.Error

import Control.Monad (mapM_, when)

class ValueOccursCheck b where
  valueOccursCheck :: String -> AST b String () -> Tc Bool

instance ValueOccursCheck BaseExpr where
  -- TRIVIAL
  valueOccursCheck var (_ :< expr) = do
    valueOccursCheckExpr var expr
    return False

valueOccursCheckExpr :: String -> ASTNode BaseExpr String () -> Tc ()
valueOccursCheckExpr _ VoidExpr = return ()
valueOccursCheckExpr _ (Literal _) = return ()
valueOccursCheckExpr _ (FnExpr _) = return ()

valueOccursCheckExpr var (ParenthesizedExpr x) = do
  valueOccursCheck var x
  return ()

valueOccursCheckExpr var (Ident names) =
  check var (last names)

valueOccursCheckExpr var (Match { expr, cases }) = do
  valueOccursCheck var expr
  mapM_ (valueOccursCheckCase var) cases

valueOccursCheckExpr var (If { ifCond, ifBody, ifElseBody }) = do
  valueOccursCheck var ifCond
  _ <- valueOccursCheck var ifBody
  _ <- valueOccursCheck var ifElseBody
  return ()

valueOccursCheckExpr var (Call { callee, args }) = do
  valueOccursCheck var callee
  _ <- mapValueOccursCheck var args
  return ()

valueOccursCheckExpr var (BinOp { lhs, op, rhs }) = do
  check op var
  valueOccursCheck var lhs
  valueOccursCheck var rhs
  return ()

valueOccursCheckExpr var (Record x) = do
  _ <- mapValueOccursCheck var $ map snd x
  return ()

valueOccursCheckExpr var (List _ items) = do
  _ <- mapValueOccursCheck var items
  return ()

valueOccursCheckExpr var (FieldAccess obj _) = do
  valueOccursCheck var obj
  return ()

valueOccursCheckExpr var (TypeCall callee _) = do
  valueOccursCheck var callee
  return ()

valueOccursCheckExpr var (Negate _ expr) = do
  valueOccursCheck var expr
  return ()

instance ValueOccursCheck BaseStmt where
  valueOccursCheck var (() :< Decl x) =
    valueOccursCheck var x

  valueOccursCheck var (() :< Expr x) =
    valueOccursCheck var x

instance ValueOccursCheck BaseCodeBlock where
  valueOccursCheck v (() :< CodeBlock stmts) =
    mapValueOccursCheck v stmts

instance ValueOccursCheck BaseDecl where
  valueOccursCheck var (() :< FnStmt (() :< Function { name })) =
    return (var == name)

  valueOccursCheck var (() :< Enum name _ _ ) =
    return (var == name)

  valueOccursCheck var (() :< Class { className }) =
    return (var == className)

  valueOccursCheck var (() :< Operator { opName }) =
    return (var == opName)

  valueOccursCheck var (() :< Interface { intfName }) =
    return (var == intfName)

  valueOccursCheck var (() :< Let (name, _) expr) =
    if var == name
       then return True
       else valueOccursCheck var expr

  valueOccursCheck _ (() :< Implementation {}) =
    return False

  valueOccursCheck _ (() :< TypeAlias {}) =
    return False

valueOccursCheckCase :: String -> Case -> Tc ()
valueOccursCheckCase var (() :< Case { pattern, caseBody }) = do
  shadowed <- valueOccursCheckPattern var pattern
  if not shadowed
     then valueOccursCheck var caseBody >> return ()
     else return ()

valueOccursCheckPattern :: String -> Pattern -> Tc Bool
valueOccursCheckPattern _ (_ :< PatDefault) = return False
valueOccursCheckPattern _ (_ :< PatLiteral _) = return False

valueOccursCheckPattern var (_ :< PatVar x) =
  return (var == x)

valueOccursCheckPattern var (_ :< PatRecord x) =
  or <$> mapM (valueOccursCheckPattern var . snd) x

valueOccursCheckPattern var (_ :< PatList items rest) = do
  sItems <- or <$> mapM (valueOccursCheckPattern var) items
  sRest <- valueOccursCheckPatternRest var rest
  return (sItems || sRest)

valueOccursCheckPattern var (_ :< PatCtor _ args) =
  or <$> mapM (valueOccursCheckPattern var) args

valueOccursCheckPatternRest :: String -> PatternRest -> Tc Bool
valueOccursCheckPatternRest _ NoRest = return False
valueOccursCheckPatternRest _ DiscardRest = return False

valueOccursCheckPatternRest var (NamedRest rest) =
  return (var == rest)

mapValueOccursCheck :: ValueOccursCheck e =>  String -> [AST e String ()] -> Tc Bool
mapValueOccursCheck _ [] =
  return False

mapValueOccursCheck var (x : xs) = do
  shadowed <- valueOccursCheck var x
  if shadowed
     then return True
     else mapValueOccursCheck var xs

check :: String -> String -> Tc ()
check v1 v2 =
  when (v1 == v2) $ throwError (VariableUsedDuringInitialization v1)
