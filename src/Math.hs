module Math where

--------------------
-- Global Imports --
import Control.Applicative

----------
-- Code --

{-|
  A data structure that contains two values. To be used with simple @'Vector'@
  math.
-}
data Vector a = Vector a a
  deriving (Eq, Show, Read)

{-|
  Functor instance for @'Vector'@.
-}
instance Functor Vector where
  fmap fn (Vector x y) = Vector (fn x) (fn y)

{-|
  Applicative instance for @'Vector'@.
-}
instance Applicative Vector where
  pure a = Vector a a
  (Vector fnx fny) <*> (Vector x y) = Vector (fnx x) (fny y)

{-|
  Adding the first element of the first vector to the first in the second, and
  the second in the first to the second in the second.

  Vector a b ^* Vector c d = Vector (a + c) (b + d)
-}
infixl 6 ^+
(^+) :: Num a => Vector a -> Vector a -> Vector a
(^+) (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

{-|
  Multiplying the first element of the first vector by the first in the second,
  and the second in the first by the second in the second.

  Vector a b ^* Vector c d = Vector (a * c) (b * d)
-}
infixl 7 ^*
(^*) :: Num a => Vector a -> Vector a -> Vector a
(^*) (Vector x1 y1) (Vector x2 y2) = Vector (x1 * x2) (y1 * y2)

{-|
  Adding a value onto all of the values contained in the vector.
-}
infixl 6 ^+>
(^+>) :: Num a => Vector a -> a -> Vector a
(^+>) v n = v ^+ pure n

{-|
  Multiplying both of the values in a Vector by a value.
-}
infixl 7 ^*>
(^*>) :: Num a => Vector a -> a -> Vector a
(^*>) v n = v ^* pure n

{-|
  Directions on a 2D plane.
-}
upDir, downDir, leftDir, rightDir :: Num a => Vector a
upDir    = Vector ( 0) ( 1)
downDir  = Vector ( 0) (-1)
leftDir  = Vector (-1) ( 0)
rightDir = Vector ( 1) ( 0)
