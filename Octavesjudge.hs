--  File     : Octavesjudge.hs
--  Author   : Danping Lei
--  Login Id : dlei
--  Student Number : 382665
--  Origin   : Tue Sep 13 2011
--  Purpose  : Judges the octaves

module Octavesjudge where

-- | Deletes all the impossible targets in the GameState according
--   to feedback of correct octaves and leaves the possible targets.
--   As correct pitch is not include in the correct octaves, so the
--   the sum of octaves and pitch should be taken as the total correct
--   octaves. Guessoctaves is a list to save all the octaves of guess,
--   and xoctaves a list to save all the octaves of each target in the
--   GameState.
octavesjudge::Num a=> a-> a->[[[Char]]]-> [[Char]] ->[[[Char]]]
octavesjudge octaves pitch [] _ = []
octavesjudge octaves pitch (x : xs) guess =  
    case sums of 0 -> if (checksub [guessoctaves!!0] xoctaves)
                        then octavesjudge octaves pitch xs guess
                        else if (checksub [guessoctaves!!1] xoctaves)
                          then octavesjudge octaves pitch xs guess
                          else if (checksub [guessoctaves!!2] xoctaves)
                            then octavesjudge octaves pitch xs guess
                            else [x] ++ (octavesjudge octaves pitch xs guess)

                 1 -> if ((checksub [guessoctaves!!0] xoctaves)
                          && (not $ checksub [guessoctaves!!1] f0_octaves)
                          && (not $ checksub [guessoctaves!!2] f0_1octaves))
                        then [x] ++ (octavesjudge octaves pitch xs guess)
                        else if ((checksub [guessoctaves!!1] xoctaves)
                             && (not $ checksub [guessoctaves!!0] f1_octaves)
                             && (not $ checksub [guessoctaves!!2] f1_0octaves))
                          then [x] ++ (octavesjudge octaves pitch xs guess)
                          else if ((checksub [guessoctaves!!2] xoctaves)
                               && (not $ checksub [guessoctaves!!0] f2_octaves)
                               && (not $ checksub [guessoctaves!!1] f2_0octaves))
                            then [x] ++ (octavesjudge octaves pitch xs guess)
                            else octavesjudge octaves pitch xs guess

                 2 -> if ((checksub [guessoctaves!!0] xoctaves)
                          && (checksub [guessoctaves!!1] f0_octaves)
                          && (not $ checksub [guessoctaves!!2] f0_1octaves))
                        then [x] ++ (octavesjudge octaves pitch xs guess)
                        else if ((checksub [guessoctaves!!0] xoctaves)
                                && (checksub [guessoctaves!!2] f0_octaves)
                                && (not $ checksub [guessoctaves!!1] f0_2octaves))
                          then [x] ++ (octavesjudge octaves pitch xs guess)
                          else if ((checksub [guessoctaves!!1] xoctaves)
                                  && (checksub [guessoctaves!!2] f1_octaves)
                                  && (not $ checksub [guessoctaves!!0] f1_2octaves))
                            then [x] ++ (octavesjudge octaves pitch xs guess)
                            else octavesjudge octaves pitch xs guess
                 
                 3 ->  if ((checksub [guessoctaves!!0] xoctaves)
                           && (checksub [guessoctaves!!1] f0_octaves)
                           && (checksub [guessoctaves!!2] f0_1octaves))
                         then [x] ++ (octavesjudge octaves pitch xs guess)
                         else octavesjudge octaves pitch xs guess

      where sums = octaves + pitch
            guessoctaves = [[(guess!!0)!!1]]++[[(guess!!1)!!1]]++[[(guess!!2)!!1]]
            xoctaves = [[(x!!0)!!1]]++[[(x!!1)!!1]]++[[(x!!2)!!1]]
            f0_octaves = filteroctaves (guessoctaves!!0) xoctaves
            f1_octaves = filteroctaves (guessoctaves!!1) xoctaves
            f2_octaves = filteroctaves (guessoctaves!!2) xoctaves
            f0_1octaves = filteroctaves (guessoctaves!!1) f0_octaves
            f1_0octaves = filteroctaves (guessoctaves!!0) f1_octaves
            f2_0octaves = filteroctaves (guessoctaves!!0) f2_octaves
            f0_2octaves = filteroctaves (guessoctaves!!2) f0_octaves
            f1_2octaves = filteroctaves (guessoctaves!!2) f1_octaves


-- | As the octaves may be repeated, the octave in the possible targets
--   should be delete after it matching the octave in the guess. This
--   function can delete one matching octave.
filteroctaves _ [] = []   
filteroctaves p (x:xs) =        
     if p/=x  
        then [x]++(filteroctaves p xs)      
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