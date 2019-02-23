{-@ LIQUID "--reflection"  @-}
{-@ LIQUID "--diff"        @-}
{-@ LIQUID "--ple"         @-}
{-@ LIQUID "--short-names" @-}

{-@ infixr ++  @-}  -- TODO: Silly to have to rewrite this annotation!

{-# LANGUAGE GADTs #-}

module Hw5 where

import           Prelude hiding ((++)) 
import           ProofCombinators
import qualified State as S
import qualified Data.Set as S
import           Expressions 
import           Imp 
import 	         BigStep

--------------------------------------------------------------------
-- | Prove that `(IF b THEN c ELSE c)` is simulated by `c`
--------------------------------------------------------------------
{-@ lem_1 :: b:_ -> c:_ -> Sim (If b c c) c @-}
lem_1 :: BExp -> Com -> SimT 
lem_1 b c s t ifbcc_s_t = impossible "TBD :: BStep" 

--------------------------------------------------------------------
-- | Prove that `c` is simulated by `(If b THEN c ELSE c)`
--------------------------------------------------------------------
{-@ lem_2 :: b:_ -> c:_ -> Sim c (If b c c) @-}
lem_2 :: BExp -> Com -> SimT 
lem_2 b c s t c_s_t = impossible "TBD :: BStep"  

--------------------------------------------------------------------------------------
-- | Prove that `WHILE b DO c` is simulated by `IF b THEN (c; WHILE b do c) ELSE Skip`
--------------------------------------------------------------------------------------
{-@ lem_3 :: b:BExp -> c:Com -> Sim (While b c) (If b (Seq c (While b c)) Skip) @-}
lem_3 :: BExp -> Com -> SimT
lem_3 b c s t wbc_s_t = impossible "TBD :: BStep" 


--------------------------------------------------------------------------------------
-- | Prove that if `cA` is simulated by `cB` then `cA; c` is simulated by `cB; c`
--------------------------------------------------------------------------------------

{-@ lem_4 :: cA:_ -> cB:_ -> c:_ -> Sim cA cB -> Sim (Seq cA c) (Seq cB c) @-}
lem_4 :: Com -> Com -> Com -> SimT -> SimT 
lem_4 cA cB c simAB s t cAc_s_t = impossible "TBD :: BStep" 

------------------------------------------------------------------------------
-- | Prove that if `c1` is simulated by `c2` 
--   then `WHILE b DO c1` is simulated by `WHILE b DO c2`
------------------------------------------------------------------------------

{-@ lem_5 :: b:_ -> c1:_ -> c2:_ -> Sim c1 c2 -> 
      Sim (While b c1) (While b c2) 
  @-}
lem_5 :: BExp -> Com -> Com -> SimT -> SimT 
lem_5 b c1 c2 sim_c1_c2 s t (BWhileF _ _ _)                
  = impossible "TBD :: BStep" 

lem_5 b c1 c2 sim_c1_c2 s t (BWhileT _ _ _ s1 _ s_s1 s1_t) 
  = impossible "TBD :: BStep" 

-------------------------------------------------------------------------------
-- | The type "AExp-Without-X" is the set of `AExp` not containing variable `X` 
-------------------------------------------------------------------------------

{-@ type AExpWithout X = {a:AExp | not (S.member X (vars a))} @-}

{-@ measure vars @-}
vars :: AExp -> S.Set Vname 
vars (N _)         = S.empty 
vars (V x)         = S.singleton x 
vars (Plus a1 a2)  = S.union (vars a1) (vars a2) 
vars (Minus a1 a2) = S.union (vars a1) (vars a2) 
vars (Times a1 a2) = S.union (vars a1) (vars a2) 

-------------------------------------------------------------------------------
-- | Prove that the value of an expression `a` (without `x`) is the 
--   same *after* assigning any value `v` to `x`.
-------------------------------------------------------------------------------

{-@ lem_same :: s:_ -> x:_ -> v:_ -> a:(AExpWithout x) -> 
      { aval a s = aval a (S.set s x v) }  
  @-}
lem_same :: State -> Vname -> Int -> AExp -> Proof 
lem_same s x v (N _)         = impossible "TBD"
lem_same s x v (V y)         = impossible "TBD"
lem_same s x v (Plus a1 a2)  = impossible "TBD"
lem_same s x v (Minus a1 a2) = impossible "TBD"
lem_same s x v (Times a1 a2) = impossible "TBD"

-------------------------------------------------------------------------------
-- | Use `lem_same` to prove that if `a` does not contain `x` then, 
--   after  assigning `a` to `x`, the value of `x` equals that of `a`. 
--   HINT: use `lemma_get_set` to deal with the "update"
-------------------------------------------------------------------------------

{-@ lem_asgn :: s:_ -> x:_ -> a:(AExpWithout x) -> 
      { aval (V x) (asgn x a s) = aval a (asgn x a s) } 
  @-} 
lem_asgn :: State -> Vname -> AExp -> Proof 
lem_asgn s x a 
  =   aval (V x) t 
  ==! aval a t 				-- complete this proof: it should work WITHOUT `==!` only using `?` and `===`
  *** QED  
  where 
    t = asgn x a s 

-------------------------------------------------------------------------------
-- | **EXTRA CREDIT** Use `lem_asgn` to prove "Imola's Equivalence", i.e. that 
---  for all `a` without `x`, the sequence `x := a; y := a` is simulated 
--   the sequence `x := a; y := x`.
-------------------------------------------------------------------------------

{-@ lem_imola :: x:_ -> y:_ -> a:AExpWithout x -> 
      Sim (Seq (Assign x a) (Assign y a)) (Seq (Assign x a) (Assign y (V x))) 
  @-}
lem_imola :: Vname -> Vname -> AExp -> SimT
lem_imola x y a s t xaya_s_t = impossible "TBD :: BStep" 
