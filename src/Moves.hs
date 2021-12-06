

module Moves where


data Move = F  | U  | L  | R  | B  | D  |
            F' | U' | L' | R' | B' | D' |
            F2 | U2 | L2 | R2 | B2 | D2
    deriving Show


-- parses String to Move
parseMove :: String -> Maybe Move
parseMove m
    | m == "F"  = Just F
    | m == "U"  = Just U
    | m == "L"  = Just L
    | m == "R"  = Just R
    | m == "B"  = Just B
    | m == "D"  = Just D
    | m == "F'" = Just F'
    | m == "U'" = Just U'
    | m == "L'" = Just L'
    | m == "R'" = Just R'
    | m == "B'" = Just B'
    | m == "D'" = Just D'
    | m == "F2" = Just F2
    | m == "U2" = Just U2
    | m == "L2" = Just L2
    | m == "R2" = Just R2
    | m == "B2" = Just B2
    | m == "D2" = Just D2
    | otherwise = Nothing



