module Typing.Ctx where

data Ctx

defaultCtx :: Ctx
deleteBetween :: Ctx -> Ctx -> Ctx -> Ctx
tImportModule :: [String] -> Ctx -> Ctx -> Ctx
