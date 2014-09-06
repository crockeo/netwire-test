module Main where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Data.IORef

-------------------
-- Local Imports --
import Network
import Math

----------
-- Code --

{-|
  The main function - the entry point of the program.
-}
main :: IO ()
main = do
  inited <- initialize
  unless inited $ error "Could not initialize GLFW."

  opened <- openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  unless opened $ error "Could not open the GLFW window."

  closedRef <- newIORef False
  windowCloseCallback $= do
    writeIORef closedRef True
    return True

  runNetwork closedRef

  closeWindow
