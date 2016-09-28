module DeSugar (desugar) where

import AST
import Type
import TypeChecker

desugar :: Program TcId -> Program TcId
desugar (Program imports decls) =
  Program imports (concatMap desugar_decl decls)

desugar_decl :: TopDecl TcId -> [TopDecl TcId]
desugar_decl (InterfaceDecl interface) =
  desugar_interface interface

desugar_decl (ImplementationDecl impl) =
  desugar_impl impl

desugar_decl decl = [decl]

desugar_interface :: Interface TcId -> [TopDecl TcId]
desugar_interface (Interface name var fns) =
  ExprDecl <$> (FunctionExpr <$> concatMap (desugar_interface_fn name) fns)

desugar_interface_fn :: TcId -> InterfaceFunction TcId -> [Function TcId]
desugar_interface_fn (TcId name _) (ConcreteFunction fn@(Function { fn_name=(Loc _ (TcId fname _)) })) =
  return fn { fn_name=loc_id (name) }

desugar_interface_fn (TcId iname _) (AbstractFunction (Prototype (TcId fname t) (FnType vars params ret_type))) =
  [Function
    (loc_id fname)
    Nothing
    ((foldl toParam [] params) ++ [FunctionParameter (loc_id "$$type$$") (length params) Nothing])
    Nothing
    (Block [
    Call
      (Call (var "at") (loc [var $ iname ++ "$" ++ fname, arg (length params)]))
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
