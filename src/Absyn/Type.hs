module Absyn.Type where
  
import Absyn.Meta

import Util.PrettyPrint

import Prelude hiding (concat)

data Type
  = TName Name
  | TApp Type [Type]
  | TArrow [Type] Type
  | TRecord [(Name, Type)]
  | TVoid
  | TPlaceholder

instance Show Type where
  show = Util.PrettyPrint.print

instance PrettyPrint Type where
  pprint (TName name) =
    str $ pprName name

  pprint (TApp ty args) =
    concat [ pprint ty
           , str "<"
           , interleave (str ", ") (map pprint args)
           , str ">"
           ]

  pprint (TArrow params ret) =
    concat [ str "("
           , interleave (str ", ") (map pprint params)
           , str ")"
           , str " -> "
           , pprint ret
           ]

  pprint (TRecord fields) =
    concat [ str "{"
           , fields'
           , str "}"
           ]
      where
        fields' = interleave (str ", ") $ map pprintField fields
        pprintField (key, ty) = concat [ str key,  str ": ", pprint ty]

  pprint TVoid =
    str "Void"

  pprint TPlaceholder =
    str "#placeholder"
