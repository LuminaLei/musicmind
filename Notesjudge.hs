--  File     : Notesjudge.hs
--  Author   : Danping Lei
--  Login Id : dlei
--  Student Number : 382665
--  Origin   : Tue Sep 13 2011
--  Purpose  : Judges the notes

module Notesjudge where

-- | Deletes all the impossible targets in the GameState according
--   to feedback of correct notes and leaves the possible targets.
--   As correct pitch is not include in the correct notes, so the
--   the sum of notes and pitch should be taken as the total correct
--   notes. Guessnotes is a list to save all the notes of guess, and
--   xnotes a list to save all the notes of each target in the GameState.
notesjudge::Num a=> a-> a->[[[Char]]]-> [[Char]] ->[[[Char]]]
notesjudge notes pitch [] _ = []
notesjudge notes pitch (x : xs) guess =  
    case sum of 0 -> if (checksub [guessnotes!!0] xnotes)
                       then notesjudge notes pitch xs guess
                       else if (checksub [guessnotes!!1] xnotes)
                         then notesjudge notes pitch xs guess
                         else if (checksub [guessnotes!!2] xnotes)
                           then notesjudge notes pitch xs guess
                           else [x] ++ (notesjudge notes pitch xs guess)

                1 -> if ((checksub [guessnotes!!0] xnotes)
                           && (not $ checksub [guessnotes!!1] f0_notes)
                           && (not $ checksub [guessnotes!!2] f0_1notes))
                       then [x] ++ (notesjudge notes pitch xs guess)
                       else if ((checksub [guessnotes!!1] xnotes)
                                  && (not $ checksub [guessnotes!!0] f1_notes)
                                  && (not $ checksub [guessnotes!!2] f0_1notes))
                         then [x] ++ (notesjudge notes pitch xs guess)
                         else if ((checksub [guessnotes!!2] xnotes)
                                    && (not $ checksub [guessnotes!!0] f2_notes)
                                    && (not $ checksub [guessnotes!!1] f2_0notes))
                           then [x] ++ (notesjudge notes pitch xs guess)
                           else notesjudge notes pitch xs guess

                2 -> if ((checksub [guessnotes!!1] xnotes)
                           && (checksub [guessnotes!!2] f1_notes)
                           && (not $ checksub [guessnotes!!0] f1_2notes))
                       then [x] ++ (notesjudge notes pitch xs guess)
                       else if ((checksub [guessnotes!!0] xnotes)
                                  && (checksub [guessnotes!!2] f0_notes)
                                  && (not $ checksub [guessnotes!!1] f0_2notes))
                         then [x] ++ (notesjudge notes pitch xs guess)
                         else if ((checksub [guessnotes!!0] xnotes)
                                    && (checksub [guessnotes!!1] f0_notes)
                                    && (not $ checksub [guessnotes!!2] f0_1notes))
                           then [x] ++ (notesjudge notes pitch xs guess)
                           else notesjudge notes pitch xs guess
                 
                3 -> if ((checksub [guessnotes!!0] xnotes)
                           && (checksub [guessnotes!!1] f0_notes)
                           && (checksub [guessnotes!!2] f0_1notes))
                       then [x] ++ (notesjudge notes pitch xs guess)
                       else notesjudge notes pitch xs guess

        where sum = notes+pitch
              guessnotes = [[(guess!!0)!!0]]++[[(guess!!1)!!0]]++[[(guess!!2)!!0]]
              xnotes = [[(x!!0)!!0]]++[[(x!!1)!!0]]++[[(x!!2)!!0]]
              f0_notes = filternotes (guessnotes!!0) xnotes
              f1_notes = filternotes (guessnotes!!1) xnotes
              f2_notes = filternotes (guessnotes!!2) xnotes
              f0_1notes = filternotes (guessnotes!!1) f0_notes
              f0_2notes = filternotes (guessnotes!!2) f0_notes
              f2_0notes = filternotes (guessnotes!!0) f2_notes
              f1_2notes = filternotes (guessnotes!!2) f1_notes


-- | As the notes may be repeated, the note in the possible targets
--   should be delete after it matching the note in the guess. This
--   function can delete one matching note.
filternotes _ [] = []   
filternotes p (x:xs) =        
     if p/=x  
        then [x]++(filternotes p xs)      
        else xs


-- | Checks if the first string is a sublist of the second one.
checksub as [] = as == []
checksub as (b:bs) =
	prefix as (b:bs) || checksub as bs
prefix [] _ = True
prefix (_:_) [] = False
prefix (a:as) (b:bs) = 
         if a == b then prefix as bs 
         else False