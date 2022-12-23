module Queue where
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)

newtype Queue a = Queue (Seq.Seq a)
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

push :: a -> Queue a -> Queue a
push a (Queue s) = Queue (s Seq.|> a)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue (first Seq.:<| rest))  = Just (first, Queue rest)
pop _ = Nothing

fromList :: [a] -> Queue a
fromList = Queue . Seq.fromList

