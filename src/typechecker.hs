module TypeChecker (type_check) where

import AST
import Type

import qualified Data.Map as Map
import Text.Printf (printf)
import Data.Foldable (foldlM)

type Context = (Map.Map String Type)
type Error = String

type_check :: AST -> (Either String Type)
type_check =  typeof Map.empty

bind :: (Either Error Type, Context) -> AST -> (Either Error Type, Context)
bind (ty, ctx) ast =
  case (ty >> typeof ctx ast) of
    Left e -> (Left e, ctx)
    Right t ->
      case ast of
        Function name _ _ _ ->
          (Right t, Map.insert name t ctx)
        _ -> (Right t, ctx)

typeof :: Context -> AST -> (Either Error Type)
typeof ctx (Program _ body) =
  fst $ foldl bind (Right TyVoid, ctx) body

typeof ctx (Block nodes) =
  fst $ foldl bind (Right TyVoid, ctx) nodes

typeof _ (Number (Left _)) = Right TyInt
typeof _ (Number (Right _)) = Right TyFloat
typeof ctx (Identifier name) =
  case Map.lookup name ctx of
    Nothing -> Left (printf "Unknown identifier: `%s`\nContext: `%s`" name (show ctx))
    Just t -> Right t

typeof ctx (BasicType t) =
  case t of
    "char" -> Right TyChar
    "int" -> Right TyInt
    "float" -> Right TyFloat
    "void" -> Right TyVoid
    "bool" -> Right TyBool
    _ -> (case Map.lookup t ctx of
           Nothing -> Left (printf "Unknown type: `%s`" t)
           Just t -> Right t)

typeof ctx (Function name params (Just ret_type) body) =
  typeof ctx body >>
  case sequence $ (typeof ctx <$> params) of
    Left e -> Left e
    Right params' ->
      (case typeof ctx ret_type of
         Left e -> Left e
         Right ret_type' -> Right (TyFunction params' ret_type'))

typeof ctx (Call callee args) =
  (typeof ctx callee) >>= \(TyFunction params ret_type) ->
    if length params /= length args then
                                    Left "Wrong number of arguments for function call"
                                    else
                                    (sequence $ typeof ctx <$> args) >>= \ty_args -> foldl (\a (t1, t2) -> (tyeqv ctx t1 t2) >> a) (Right ret_type) (zip params ty_args)

typeof ctx (FunctionParameter _ _ (Just t)) =
  typeof ctx t

typeof ctx t =
  Left ("Unhandled node: " ++ (show t))

tyeqv :: Context -> Type -> Type -> Either Error ()
tyeqv ctx t1 t2 =
  case (t1, t2) of
    (TyChar, TyChar) -> Right ()
    (TyInt, TyInt) -> Right ()
    (TyFloat, TyFloat) -> Right ()
    (TyVoid, TyVoid) -> Right ()
    (TyBool, TyBool) -> Right ()
    (_, _) -> Left "Invalid type for argument"
