module Main where

import Prelude hiding (lex)
import Data.Char
import ParseLib

-- placeholders
type TODO x = x
tODO x = x

-- --------- --
-- Utilities --
-- --------- --

-- Running a parser.
run :: Parser a b -> [a] -> b
run = tODO (error "not yet defined")


-- ----- --
-- Moves --
-- ----- --

-- Feel free to change type synonyms into datatypes
type Move = TODO ()

-- A parser for SAN moves.
parseMove :: Parser Char Move
parseMove = tODO (succeed ())

-- Testing a move.
promotionCheck :: Move -> Bool
promotionCheck m = tODO False

-- ----- --
-- Games --
-- ----- --

-- Feel free to change type synonyms into datatypes
type PGN = TODO ()

-- A parser for PGN files. Alternatively, you can choose
-- to define a lexical analyzer and turn this into a
-- Parser Token PGN ...
parsePGN :: Parser Char PGN
parsePGN = tODO (succeed ())

-- Reading PGN files.
readPGN :: String -> IO PGN
readPGN f = tODO (return ())

-- Collect game statistics. You have to choose a suitable
-- type.
statistics :: PGN -> TODO ()
statistics pgn = tODO ()

