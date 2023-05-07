{-# LANGUAGE GADTSyntax #-}

import           Beaver (deck, doBeaver)
import           System.Console.Haskeline
import           Data.List                (isPrefixOf)
import           Data.Char                (isDigit)
import           GHC.Word                 (Word8)
import           Control.Monad.State

description :: String
description = "Welcome to The Busy Beaver Sandbox!" ++ [toEnum 129451] ++ [toEnum 129451] ++ [toEnum 129451]

helpMsg :: String
helpMsg = unlines
  [ ":syntax      - get a nice guide on how to format a beaver deck"
  , ":card        - a comprehensive explanation of the components of a beaver card, and tips on how to make them halt."
  , ":help        - print this message."
  , ":quit        - quit."
  , ""
  , "Your command history is automatically saved in a file named quilt_history.txt."
  ]

syntaxMsg :: String
syntaxMsg = unlines
    [ "The proper format for a beaver deck is as follows:"
    , "  [                                      ] <- {square brackets}"
    , "   (D0 M1 2, D1 M0 1), (D0 M0 1, D1 M1 1) <- {cards have two sets}"
    , "             or, alternatively:"
    , "       (0 1 2, 1 0 1), (0 0 1, 1 1 0) <- {spaces ARE necessary}" 
    , ""
    , "Valid inputs can have a mixture of digits or commands, so"
    , "    [(D0 1 2, 0 M0 0)] is a valid input."
    , ""
    , "Additionally, D-commands and M-commands may not switch places,"
    , "nor can they switch places with the end digit." 
    , "    [(M0 0 D1, 0 D1 M1)] is not a valid input."
    ]

cardMsg :: String
cardMsg = unlines
    [ "Busy Beaver cards are instruction cards for a turing machine"
    , ""
    , "They include two instruction sets, they are chosen based on the current value on the tape."
    , "In these instruction sets, there are three instructions."
    , ""
    , " * The first is an instruction fow writing either a 1 or 0 on the tape"
    , ""
    , " * The second is an instruction that shifts the tape left or right, 0 for left and 1 for right"
    , ""
    , " * The third is an integer that indicates which instruction card to consult next"
    , ""
    , "Each instruction is required for a valid instruction set," 
    , "and two instruction sets are required for a valid instruction card."
    , "The first instruction set is used if the beaver is currently on a 0,"
    , "and the second if the beaver is on a 1"
    , ""
    , ""
    , "that's all for rules, here's some tips:"
    , ""
    , " * Make sure one of your cards has a 0 for the third instruction."
    , "   This is necessary for the beaver to halt; if you don't, your"
    , "   beaver will work forever and it's likely your computer will"
    , "   run out of memory to accommodate your relentlessly busy beaver."
    , ""
    , " * Make sure there is a possibility of your halt instruction being"
    , "   called. If you have, three cards in the beaver deck, and the third"
    , "   is the one with the halt command, but the third card isn't referenced"
    , "   by your other cards, then it's still no good!"
    , ""
    , " * Finally, refining that last tip, make sure all of your cards are linked"
    , "   together in some way or another! The beaver starts with the first card"
    , "   in the deck, so make sure all of your cards are directly or indirectly"
    , "   accessible from the first card."
    , ""
    , "That's all there is to say. Now get busy, you beaver!"
    ]

eval :: String -> String
eval s = doBeaver (deck s)

beaverREPL :: IO ()
beaverREPL = putStrLn description >> runInputT settings loop
    where
        settings = defaultSettings { historyFile = Just ".beaver-hist" }

loop :: InputT IO ()
loop = do
    minput <- getInputLine "> "
    case minput of
        Nothing      -> return ()
        Just s | s `isPrefixOf` ":quit" -> return ()
            | s `isPrefixOf` ":help" -> (outputStrLn $ helpMsg) >> loop
            | s `isPrefixOf` ":syntax" -> (outputStrLn $ syntaxMsg) >> loop
            | s `isPrefixOf` ":card" -> (outputStrLn $ cardMsg) >> loop
        Just input   -> do
            outputStrLn $ eval input
            loop




main :: IO ()
main = beaverREPL