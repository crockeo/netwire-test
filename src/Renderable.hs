module Renderable where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Control.Monad

-------------------
-- Local Imports --
import Math

----------
-- Code --

{-|
  A type synonym to suggest that renders should be rendering and not performing
  other IO.
-}
type Scene = IO ()

{-|
  A typeclass to see if something can be rendered in OpenGL.
-}
class Renderable a where
  render :: a -> Scene

{-|
  Rendering a point (in @'Vector'@ form).
-}
s :: RealFrac a => a
s = 0.05

generatePoints :: RealFrac a => Vector a -> [Vector a]
generatePoints (Vector x y) = [ Vector (x - s) (y - s)
                              , Vector (x + s) (y - s)
                              , Vector (x + s) (y + s)
                              , Vector (x - s) (y + s)
                              ]

instance RealFrac a => Renderable (Vector a) where
  render v =
    renderPrimitive Quads $
      forM_ (generatePoints v) $ \(Vector x y) ->
        vertex $ Vertex2 (realToFrac x :: GLfloat) (realToFrac y :: GLfloat)
