module TypedValue where

import qualified Util.PrettyPrint as PP

typedValue :: (Show a, PP.PrettyPrint b) => a -> b -> String
typedValue val ty =
  show $ PP.concat [PP.str (show val), PP.str " : ", PP.pprint ty]
