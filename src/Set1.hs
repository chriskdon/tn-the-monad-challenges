{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = fiveRands' $ mkSeed 1

fiveRands' :: Seed -> [Integer]
fiveRands' seed = 
  let a = rand seed
      b = rand $ snd a
      c = rand $ snd b
      d = rand $ snd c
      e = rand $ snd d
  in [fst a, fst b, fst c, fst d, fst e]

randString3 :: (String, Seed)
randString3 = 
    let a = randLetter $ mkSeed 1
        b = randLetter $ snd a
        c = randLetter $ snd b
    in ([fst a, fst b, fst c], snd c)

randMap :: (a -> b) -> Gen a -> Gen b
randMap f rvg seed = 
  let res = rvg seed
      val = f $ fst res
      nxt = snd res
  in (val, nxt)

randLetter:: Gen Char
randLetter = randMap toLetter rand

randEven :: Gen Integer
randEven = randMap (* 2) rand

randOdd :: Gen Integer
randOdd = randMap (\x -> x * 2 + 1) rand

randTen :: Gen Integer
randTen = randMap (* 10) rand

randPair :: Gen (Char, Integer)
randPair seed = 
  let char = randLetter seed
      int  = rand $ snd char
  in ((fst char, fst int), snd int)

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb seed =
  let a         = ga seed
      b         = gb $ snd a
      av        = fst a
      bv        = fst b
      nextSeed  = snd b
  in (f av bv, nextSeed)

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair fa fb seed = 
  let a = fa seed
      b = fb $ snd a
  in ((fst a, fst b), snd b)

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

repRandom :: [Gen a] -> Gen [a]
repRandom gfs seed = foldr apply root gfs
  where root  = ([], seed)
        apply = \fg (ls, s) -> let (v, ns) = fg s 
                               in (ls ++ [v], ns) -- Improve (don't use (++) ?)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo fg fm seed =
    let (av, as) = fg seed
        (bv, bs) = fm av as
    in (bv, bs)

mkGen :: a -> Gen a
mkGen v seed = (v, seed)