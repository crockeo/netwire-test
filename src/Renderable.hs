module Renderable where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL

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
instance Real a => Renderable (Vector a) where
  render (Vector x y) =
    renderPrimitive Points $
      vertex $ Vertex2 (realToFrac x :: GLfloat) (realToFrac y :: GLfloat)
