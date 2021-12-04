module Main where

import qualified Day15

-- type family RPN op

-- -- data Op = Push | End | Add | I
-- data Push = Push

-- data End = End

-- data Add = Add

-- -- data I = I

-- type instance RPN Push cont = Int -> cont

-- type instance RPN Int (Int -> cont) = cont

-- type instance RPN Add (Int, (Int, a)) = (Int, a)

-- type instance RPN End (Int, a) = Int

-- push = Push
-- end = End
-- add = Add

-- begin :: a -> RPN a b
-- begin Push = \i -> ()
-- begin Add =

-- begin :: (Int -> (Int -> cont) -> cont) -> Int -> (Int -> cont) -> cont
-- begin = id
begin x y = y

push x y = x

add x y z = x z (y z)

end = id

-- -- push :: (Int -> cont) -> Int -> cont
-- -- push i = i
-- push :: Int -> (Int -> cont) -> cont
-- push i c = c i

-- -- add :: (Int -> cont) -> Int -> Int -> cont
-- -- add c i1 i2 = c (i1 + i2)
-- add :: (Int -> cont) ->  cont
-- add = id

-- end :: Int -> Int
-- end i = i

-- begin ::
-- begin push  :: Int -> (* -> *)
-- begin push 5  :: (* -> *)
-- begin push 5 push :: Int -> (* -> *)
-- begin push 5 push 6 :: (* -> *)
-- begin push 5 push 6 add  :: (* -> *)
-- begin push 5 push 6 add end  === begin push 11 end :: Int
--
-- begin push 5 === begin push 2 push 3 add
-- test:: Int
-- test = begin push 5 push 6 add end
-- test2 = begin push 5 end
-- begin :: push -> Int -> end -> Int
-- begin push :: Int -> end -> Int
-- begin push 5 :: end -> Int
-- begin push 5 end :: Int

-- push' :: Int -> (Int -> cont) -> cont
-- push' :: (forall stash. stash -> r) -> stash -> Int -> r
-- push' f s i = f (i, s)
push' :: (Int, s) -> ((Int, s) -> next) -> next
push' v f = f v

end' :: (Int, stash) -> a -> Int
end' (x, y) _ = x

-- begin' :: (x -> a) -> x -> (a -> b) -> b
-- begin' f x c= c (f x)
-- begin' f a  = f a (begin' f)
-- begin' f a = f a undefined
fix' f = f (fix' f)

-- begin' :: (x -> y) -> x -> (y -> z) -> z
-- begin' f x g = g (f x)
-- begin' :: (a -> b -> 1)
-- begin' f x y = f x (begin' y)

-- begin should take whatever the function is doing, take all of the inputs needed and then do the next thing

-- begin' :: (forall s. (a, s) -> ((b, s) -> c) -> c) -> a -> (forall s. (b, s) -> c) -> c
begin' :: (forall s1. x -> s1 -> s2) -> x -> (forall s2. s2 -> s3) -> s3
begin' f s next =
  let newS = f s 0
   in next newS

-- begin' f a z = f begin' a
-- begin' :: (x -> Int -> z) -> Int -> (z -> Int -> y) -> y
-- begin' f a z = f (begin' z) a

-- y f  = (\x -> f (x x)) (\x -> f (x x))

add' :: (Int, (Int, stash)) -> (Int, stash)
add' (i, (y, r)) = (i + y, r)

-- dupa :: Int
dupa = begin' push' 6

dupa1 = end' (push' 0 5)

dup2 = begin' push' 5 push' 6

-- begin' push' 5 -> push' 5 (begin' push') -> begin' push' 5

-- dupa2 :: Int
-- dupa2 = begin' push' 5 push' 6 add' end'

-- push enforces Int
-- after Int there has to be something
-- add: does it enforce anything?

-- end' f x y z = z (f x y)

-- -- add' :: Int -> Int -> Int
-- add' a b = (a + b)

-- begin' = id
-- test3 = end' add' 6 5 begin'
-- test4 = end' push' 5 begin'

main :: IO ()
main = Day15.main
