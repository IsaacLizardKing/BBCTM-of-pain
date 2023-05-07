{-# LANGUAGE GADTSyntax #-}

module Beaver where
import qualified Data.Map as M 
import qualified Text.Parsec.Error
import           Parsing2  hiding ((<$), (<$>), (<*>), (*>), (<*))
import           Prelude

type Cards = [InstructionCard]
type Depth = Int
type Index = Integer
type Perry = Text.Parsec.Error.ParseError
type InstructionCard = (InstructionSet, InstructionSet)

data LeftTape where
    Link :: Bool -> LeftTape -> LeftTape
    Lend :: LeftTape
    deriving (Show)

data RightTape where
    Rink :: Bool -> RightTape -> RightTape
    Rend :: RightTape
    deriving (Show)

data CenterTape where
    Origin :: Bool -> LeftTape -> RightTape -> CenterTape
    deriving (Show)

data InstructionSet where
    Read :: WriteInstruction -> MoveInstruction -> Index -> InstructionSet
    Halt :: InstructionSet
    deriving (Show)

data WriteInstruction where 
    D0 :: WriteInstruction
    D1 :: WriteInstruction
    deriving (Show)

data MoveInstruction where 
    M0 :: MoveInstruction
    M1 :: MoveInstruction
    deriving (Show)

lexer :: TokenParser u
lexer = makeTokenParser $ emptyDef { reservedNames = (["Halt"])}

parens  :: Parser a -> Parser a
parens     = getParens lexer

reserved :: String -> Parser ()
reserved   = getReserved lexer

whiteSpace :: Parser ()
whiteSpace = getWhiteSpace lexer

integer :: Parser Integer
integer = getInteger lexer

moveinstruction :: Parser MoveInstruction
moveinstruction   = M0 <$ (reserved "0")
                <|> M0 <$ (reserved "M0")
                <|> M0 <$ (reserved "l")
                <|> M1 <$ (reserved "1")
                <|> M1 <$ (reserved "M1")
                <|> M1 <$ (reserved "r")

writeinstruction :: Parser WriteInstruction
writeinstruction  = D0 <$ (reserved "0")
                <|> D0 <$ (reserved "D0")
                <|> D1 <$ (reserved "1")
                <|> D1 <$ (reserved "D1")

instruction :: Parser InstructionSet
instruction = whiteSpace *> (Read <$> writeinstruction <*> moveinstruction <*> integer) <* whiteSpace

instructioncard :: Parser InstructionCard
instructioncard = do
    char '('
    case1 <- instruction
    reserved "," 
    case2 <- instruction
    char ')'
    return (case1, case2)

parsecards :: Parser Cards
parsecards = do
    char '['
    cards <- (whiteSpace *> instructioncard <* whiteSpace) `sepBy` (reserved ",")
    char ']'
    return cards

cardMachine :: Parser Cards
cardMachine = whiteSpace *> parsecards <* eof

-- I was just running out of names at this point 
-- so I just opted for something silly.

deck :: String -> Cards
deck s = case parse cardMachine s of
    (Left s) -> [(Halt, Halt)]
    (Right c) -> c

getInstruction :: Cards -> Index -> InstructionCard
getInstruction [c] _ = c
getInstruction (c : _) 0 = c
getInstruction (_ : c) i = getInstruction c (i - 1)

shiftTape :: MoveInstruction -> CenterTape -> CenterTape
shiftTape M1 (Origin b l (Rend)) = Origin False (Link b l) Rend  -- shift tape right and extend
shiftTape M0 (Origin b (Lend) r) = Origin False Lend (Rink b r)  -- shift tape left  and extend
shiftTape M1 (Origin b1 l (Rink b2 r)) = Origin b2 (Link b1 l) r -- shift tape right 
shiftTape M0 (Origin b1 (Link b2 l) r) = Origin b2 l (Rink b1 r) -- shift tape left

beaverStep :: InstructionCard -> CenterTape -> (Index, CenterTape)
beaverStep (Halt, Halt) t                      = (0, t)
beaverStep (Read D0 m i, _) (Origin False l r) = (i, shiftTape m (Origin False l r))
beaverStep (Read D1 m i, _) (Origin False l r) = (i, shiftTape m (Origin True  l r))
beaverStep (_, Read D0 m i) (Origin True  l r) = (i, shiftTape m (Origin False l r))
beaverStep (_, Read D1 m i) (Origin True  l r) = (i, shiftTape m (Origin True  l r))

-- Performs a step of the beaver game, shifts the tape and returns

tally :: CenterTape -> Integer
tally (Origin b Lend Rend)                  = (if b then 1 else 0)
tally (Origin b1 (Link b2 l) Rend)          = (if b2 then 1 else 0) + tally (Origin b1 l Rend)
tally (Origin b1 Lend (Rink b2 r))          = (if b2 then 1 else 0) + tally (Origin b1 Lend r)
tally (Origin b1 (Link b2 l) (Rink b3 r))   = (if b2 then 1 else 0) + (if b3 then 1 else 0) + tally (Origin b1 l r)

-- consumes the tape from the middle and tallies the ones


execBeaver :: Cards -> Index -> CenterTape -> Depth -> (CenterTape, Integer, Depth)
execBeaver c i1 t1 steps = case beaverStep (getInstruction c i1) t1 of
    (0, t2) -> (t2, tally t2, steps)
    (i2, t2) -> execBeaver c i2 t2 (steps + 1)

-- Plays the beaver game until halt instruction is called

showLTape :: LeftTape -> String
showLTape Lend       = ""
showLTape (Link b l) = (showLTape l) ++ "| " ++ (if b then "1" else "0") ++ " "

showRTape :: RightTape -> String
showRTape Rend       = ""
showRTape (Rink b r) = " " ++ (if b then "1" else "0") ++ " |" ++ (showRTape r)

showTape :: CenterTape -> String
showTape (Origin b Lend Rend)                  = " " ++ (if b then "1" else "0") ++ [toEnum 129451]
showTape (Origin b1 l r)   = (showLTape l) ++ "|" ++ showTape (Origin b1 Lend Rend) ++ "|" ++ (showRTape r)

-- Creates a string representing the tape with a beaver emoticon to the right of the bit it stopped on

showBeaver :: (CenterTape, Integer, Depth) -> String
showBeaver (t, i, d) = unlines 
    [ "score = " ++ (show i)
    , "depth = " ++ (show d)
    , "tape:"
    , (showTape t)
    ]

-- General scoreboard for beaver.

doBeaver :: Cards -> String
doBeaver c = showBeaver (execBeaver ([(Halt, Halt)] ++ c ++ [(Halt, Halt)]) 1 (Origin False Lend Rend) 0)
