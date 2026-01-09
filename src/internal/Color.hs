module Color where

data Color = Black | White
    deriving (Eq,Ord,Bounded,Ix,Show)

oppColor Black = White
oppColor White = Black

showColor :: Color -> String
showColor Black = "b"
showColor White = "w"

readColor :: String -> Color
readColor "b" = Black
readColor "w" = White
