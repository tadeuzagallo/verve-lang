module Section where

header :: Integer
header = 0xCE05

function_header :: Integer
function_header = 0xCE0F

data Section = Strings
             | Functions
             | Text
             | InstanceMaps
             deriving (Show, Enum)
