module DeSugar (desugar) where

import AST
import Type
import TypeChecker

import Control.Monad (liftM)
import Control.Monad.Reader (Reader, runReader)

type DsState = Reader Context

desugar :: Context -> Program TcId -> Program TcId
desugar ctx program =
  runReader (desugar_program program) ctx

desugar_program :: Program TcId -> DsState (Program TcId)
desugar_program (Program imports decls) = do
  decls' <- mapM desugar_decl decls
  return $ Program imports (concat decls')

desugar_decl :: TopDecl TcId -> DsState [TopDecl TcId]
desugar_decl (InterfaceDecl interface) =
  return $ desugar_interface interface

desugar_decl (ImplementationDecl impl) =
  return $ desugar_impl impl

desugar_decl (ExprDecl expr) = do
  exprs <- desugar_expr expr
  return $ ExprDecl <$> exprs

desugar_decl decl = return [decl]

desugar_expr :: Expr TcId -> DsState [Expr TcId]
desugar_expr (Call callee@(Var (Loc _ (TcId _ (TyAbstractFunction _ _)))) (Loc pos args)) = do
  let args' = args ++ [LiteralExpr $ Number $ Left 0x42]
  (callee':_) <- desugar_expr callee
  return [Call callee' (Loc pos args')]

desugar_expr expr@(Call callee (Loc pos args)) = do
  (callee':_) <- desugar_expr callee
  args' <- mapM desugar_expr args
  return [Call callee' (Loc pos $ concat args')]

desugar_expr expr = return $ [expr]

desugar_interface :: Interface TcId -> [TopDecl TcId]
desugar_interface (Interface name var fns) =
  ExprDecl <$> (FunctionExpr <$> concatMap (desugar_interface_fn name) fns)

desugar_interface_fn :: TcId -> InterfaceFunction TcId -> [Function TcId]
desugar_interface_fn (TcId name _) (ConcreteFunction fn@(Function { fn_name=(Loc _ (TcId fname _)) })) =
  return fn { fn_name=loc_id fname }

desugar_interface_fn (TcId iname _) (AbstractFunction (Prototype (TcId fname t) (FnType vars params ret_type))) =
  [Function
    (loc_id fname)
    Nothing
    ((foldl toParam [] params) ++ [FunctionParameter (loc_id "$$type$$") (length params) Nothing])
    Nothing
    (Block [
    Call
      (Call (var "type$map") (loc [var $ iname ++ "$" ++ fname, arg (length params)]))
      (loc $ foldl toArg [] params) ])]
        where toParam a t = a ++ [param $ length a]
              toArg a t = a ++ [arg $ length a]

desugar_impl :: Implementation TcId -> [TopDecl TcId]
desugar_impl (Implementation (Loc _ (TcId iname t)) _ fns) =
  ExprDecl <$> (FunctionExpr <$> concatMap (desugar_impl_fn iname (show t)) fns)

desugar_impl_fn :: String -> String -> ImplementationFunction TcId -> [Function TcId]
desugar_impl_fn _ _ (ExternImplementation {}) = []

desugar_impl_fn iname tname (LocalImplementation fn@(Function { fn_name=(Loc _ (TcId fname _)) })) =
  return fn { fn_name=loc_id (iname ++ "$" ++ tname ++ "$" ++ fname) }


-- Helpers
dummy_loc :: SourcePos
dummy_loc = (SourcePos 0 0 "")

tcid :: String -> TcId
tcid n = TcId n TyVoid

loc :: a -> Loc a
loc id = Loc dummy_loc id

loc_id :: String -> Loc TcId
loc_id = loc . tcid

var :: String -> Expr TcId
var n = Var (loc_id n)

arg :: Int -> Expr TcId
arg index = Arg (loc_id "") index

param :: Int -> FunctionParameter TcId
param index = FunctionParameter (loc_id "") index Nothing
