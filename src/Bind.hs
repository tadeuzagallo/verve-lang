module Bind  where 

import AST

data Bind id
  = ParamBind (FunctionParameter id)
  | FunBind (Function id)
