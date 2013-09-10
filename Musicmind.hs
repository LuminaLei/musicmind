--  File     : Musicmind.hs
--  Author   : Danping Lei
--  Login Id : dlei
--  Student Number : 382665
--  Origin   : Tue Sep 13 2011
--  Purpose  : Implement a guessing game Musicmind

-- | This program uses the way of Enumeration Delete to solve 
--   the Musicmind guessing game. A list with 1330 possible
--   targets would be built at the beginning. The the functions
--   would delete all the impossible targets saving in the
--   GameState according to feedback of correct pitch, notes,
--   and octaves, and leaves the possible targets. When all
--   the impossible targets have been deleted, the result
--   should be the target.

module Musicmind where
import Pitchjudge
import Notesjudge
import Octavesjudge
import Data.List

-- | Stores the possibile results in GameState.
type GameState =  [[String]]


-- | Builds a list of chords and order the list with function orderlist.
initiallist =
    orderlist triplechord
       where singlechord = [[x,y]|x<-"ABCDEFG",y<-"123"]
             triplechord = [[x,y,z]|x<-singlechord,y<-singlechord,
                                                   z<-singlechord]


-- | Orders the chords list and filter out the duplicate chords.
orderlist :: [[String]] -> [[String]]
orderlist [] = [] 
orderlist (x : xs) = 
   if (head x)<(x !! 1) && (x !! 1)<( x !! 2)
       then [ x ] ++ (orderlist xs)
   else orderlist xs


-- | Takes no input arguments and returns an initial guess and
--   the GameState. Set the initial guess as ["A1", "B2", "C3"].
initialGuess :: ([String], GameState)
initialGuess = (["A1", "B2", "C3"], initiallist)


-- | Takes the previous guess and GameState, the correct pitches,
--   notes, and octaves from the Main program. Returns the next 
--   guess and the next GameState (delete some of the chords according
--   to feedback) to the Main program.
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (guess, other) (a, b, c) = (guess', other')                                
                     where other' = octavesjudge c a othernotes guess
                           othernotes = notesjudge b a otherpitch guess
                           otherpitch = pitchjudge a other guess
                           guess' = head other'