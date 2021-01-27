module ParseState where
-- Keeps track of all information needed while parsing. Some redundancy for speed.

data ParseState = ParseState { line   :: Int
                             , column :: Int
                             , pos    :: Int
                             , code   :: String
                             , rest   :: String
                             } deriving Show

-- Shows the current parsestate using its line, column, and showing/pointing to the character in the line
showPos :: ParseState -> String
showPos pos = "Line " ++ (show $ line pos) ++ ", Column " ++ (show $ column pos)
               ++ "\n    " ++ lineStr pos ++ "\n   " ++ replicate (column pos) ' ' ++ "^"

-- Default parseState
parseState :: String -> ParseState
parseState code = ParseState 1 1 1 code code

-- Increases the position, including columns and newline if necessary
inc :: ParseState -> ParseState
inc ps = if nextChar ps == '\n' then incLine ps else incColumn ps

incColumn :: ParseState -> ParseState
incColumn (ParseState line col pos code rest) = ParseState line (col+1) (pos+1) code (tail rest)

incLine :: ParseState -> ParseState
incLine (ParseState line _ pos code rest) = ParseState (line+1) 1 (pos+1) code (tail rest)

-- Gets the next character, can error
nextChar :: ParseState -> Char
nextChar ps = head $ rest ps

-- Checks next character exists
hasNextChar :: ParseState -> Bool
hasNextChar ps = length (rest ps) > 0

-- Builds a full parsestate from a string and position (calculating column and line number)
fromPos :: String -> Int -> ParseState
fromPos code pos = ParseState { code   = code
                              , pos    = clampedPos
                              , line   = (length $ filter (== '\n') prePosCode) + 1
                              , column = column
                              , rest   = drop clampedPos code }
                        where
                            clampedPos = min pos $ length code
                            prePosCode = take clampedPos code
                            column     = length $ takeWhile (/= '\n') $ reverse prePosCode

-- Gets the full current line
lineStr :: ParseState -> String
lineStr (ParseState _ column pos code _) = takeWhile (/= '\n') (drop (pos - column) code)
