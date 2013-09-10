--  File     : Pitchjudge.hs
--  Author   : Danping Lei
--  Login Id : dlei
--  Student Number : 382665
--  Origin   : Tue Sep 13 2011
--  Purpose  : Judges the pitch

module Pitchjudge where

-- | Deletes all the impossible targets in the GameState according
--   to feedback of correct pitches and leaves the possible targets.
--   For example, when pitch is zero, deletes all the targets that
--   have any same chord with the guess. When pitch is one, keeps
--   the targets that just have one same chord with the guess. When
--   pitch is two, keeps the targets that have two same chords with
--   the guess.
pitchjudge::Num a=> a->[[[Char]]] -> [String] ->[[[Char]]]
pitchjudge _ [] [] = []
pitchjudge _ [] _ = []
pitchjudge pitch (x : xs) guess =
        case pitch of 0 -> if (checksub [guess!!0] x)
                             then pitchjudge pitch xs guess
                             else if (checksub [guess!!1] x)
                               then pitchjudge pitch xs guess
                               else if (checksub [guess!!2] x)
                                 then pitchjudge pitch xs guess
                                 else [x] ++ (pitchjudge pitch xs guess)

                      1 -> if (checksub [guess!!0] x)
                                   && (not $ checksub [guess!!1] x)
                                   && (not $ checksub [guess!!2] x)
                             then [x] ++ (pitchjudge pitch xs guess)
                             else if (checksub [guess!!1] x)
                                     && (not $ checksub [guess!!0] x)
                                     && (not $ checksub [guess!!2] x)
                               then [x] ++ (pitchjudge pitch xs guess)
                               else if (checksub [guess!!2] x)
                                       && (not $ checksub [guess!!0] x)
                                       && (not $ checksub [guess!!1] x)
                                 then [x] ++ (pitchjudge pitch xs guess)
                                 else pitchjudge pitch xs guess

                      2 -> if (checksub [guess!!1] x)
                                   && (checksub [guess!!2] x)
                                   && (not $ checksub [guess!!0] x)
                             then [x] ++ (pitchjudge pitch xs guess)
                             else if (checksub [guess!!0] x)
                                     && (checksub [guess!!2] x)
                                     && (not $ checksub [guess!!1] x)
                               then [x] ++ (pitchjudge pitch xs guess)
                               else if (checksub [guess!!0] x)
                                       && (checksub [guess!!1] x)
                                       && (not $ checksub [guess!!2] x)
                                 then [x] ++ (pitchjudge pitch xs guess)
                                 else pitchjudge pitch xs guess


-- | Checks if the first string is a sublist of the second one.
checksub as [] = as == []
checksub as (b:bs) =
	prefix as (b:bs) || checksub as bs
prefix [] _ = True
prefix (_:_) [] = False
prefix (a:as) (b:bs) = 
         if a == b then prefix as bs 
         else False
