module Util.PrettyPrint
  ( PrettyPrint(..)
  , Util.PrettyPrint.print
  , Out
  , pprName

  , nil
  , str
  , num
  , append
  , newline
  , indent
  , Util.PrettyPrint.concat
  , interleave
  ) where

-- Data type for Output
data Out
  = Str String
  | Newline
  | Indent Out
  | Nil
  | Append Out Out

-- helpers
nil :: Out
nil = Nil

str :: String -> Out
str = Str

num :: (Num n, Show n) => n -> Out
num n = str (show n)

append :: Out -> Out -> Out
append = Append

newline :: Out
newline = Newline

indent :: Out -> Out
indent = Indent

concat :: [Out] -> Out
concat = foldr append Nil

interleave :: Out -> [Out] -> Out
interleave _ []  = Nil
interleave _ [o] = o
interleave sep (o : os) = (o `append` sep) `append` interleave sep os

-- Class for PrettyPrinting with Out
class PrettyPrint a where
  pprint :: a -> Out

-- Printing Out
instance Show Out where
  show out = flatten 0 [(out, 0)]

-- Idempotent
instance PrettyPrint Out where
  pprint a = a

print :: PrettyPrint a => a -> String
print = show . pprint

flatten :: Int -> [(Out, Int)] -> String
flatten _ [] = ""
flatten _ ((Newline, indent) : out) = '\n' : spaces indent ++ flatten indent out
flatten col ((Str s, _) : out) = s ++ flatten col out
flatten col ((Indent o, _) : out) = flatten col ((o, col + 1) : out)
flatten col ((Nil, _) : out) = flatten col out
flatten col ((Append o1 o2, indent) : out) = flatten col ((o1, indent) : (o2, indent) : out)

spaces :: Int -> String
spaces n = replicate (n * indent_size) ' '

indent_size :: Int
indent_size = 2

-- should be (re)moved
pprName :: String -> String
pprName = reverse . takeWhile (/= '.') . reverse
