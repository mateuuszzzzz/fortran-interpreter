module Helpers (isAlpha, isDigit) where 



isDigit :: Char -> Bool
isDigit d = d=='0' || d=='1' || d=='2' || d=='3' || d=='4' || d=='5' || d=='6' || d=='7' || d=='8' || d=='9'

isAlpha :: Char -> Bool
isAlpha l = l== 'a' || l== 'b' || l== 'c' || l== 'd' || l== 'e' || l== 'f' || l== 'g' || l== 'h' || l== 'i' || l== 'j' || l== 'k' || l== 'l' || l== 'm' || l== 'n' || l== 'o' || l== 'p' || l== 'q' || l== 'r' || l== 's' || l== 't' || l== 'u' || l== 'v' || l== 'w' || l== 'x' || l== 'y' || l== 'z' || l== 'A' || l== 'B' || l== 'C' || l== 'D' || l== 'E' || l== 'F' || l== 'G' || l== 'H' || l== 'I' || l== 'J' || l== 'K' || l== 'L' || l== 'M' || l== 'N' || l== 'O' || l== 'P' || l== 'Q' || l== 'R' || l== 'S' || l== 'T' || l== 'U' || l== 'V' || l== 'W' || l== 'X' || l== 'Y' || l== 'Z'