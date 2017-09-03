module Absyn.Meta where

type Name = String

data Import = Import
  { iGlobal :: Bool
  , iModule :: [Name]
  , iAlias :: Maybe Name
  , iItems :: Maybe [ImportItem]
  }

data ImportItem
  = ImportValue Name
  | ImportType Name [Name]

data Associativity
  = AssocNone
  | AssocLeft
  | AssocRight

instance Show Associativity where
  show AssocNone = "none"
  show AssocLeft = "left"
  show AssocRight = "right"

defaultAssoc :: Associativity
defaultAssoc = AssocLeft

data Precedence
  = PrecHigher Name
  | PrecLower Name
  | PrecEqual Name
  | PrecValue Integer

instance Show Precedence where
  show (PrecHigher name) = "higher(" ++ name ++ ")"
  show (PrecLower name) = "lower(" ++ name ++ ")"
  show (PrecEqual name) = "equal(" ++ name ++ ")"
  show (PrecValue n) = show n

defaultPrec :: Precedence
defaultPrec = PrecValue 50

data Literal
  = Integer Integer
  | Float Double
  | Char Char
  | String String
  deriving (Eq)

instance Show Literal where
  show (Integer i) = show i
  show (Float i) = show i
  show (Char i) = show i
  show (String i) = show i
