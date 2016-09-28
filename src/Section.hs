module Section where

header :: Integer
header = 0xCE05

function_header :: Integer
function_header = 0xCE0F

type_map_header :: Integer
type_map_header = 0xCE01

data Section = Strings
             | TypeMaps
             | Functions
             | Text
             deriving (Show, Enum)
