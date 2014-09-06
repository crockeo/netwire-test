module Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Control.Wire
import Data.IORef

-------------------
-- Local Imports --
import Renderable
import Math

----------
-- Code --

{-|
  The speed in which the dot should move.
-}
speed :: Float
speed = 0.1

{-|
  The initial position.
-}
initPos :: Vector Float
initPos = Vector 0 0

{-|
  The time since the program started.
-}
timeFloat :: HasTime t s => Wire s e IO a Float
timeFloat = timeF

{-|
  The moving position of the dot.
-}
movPos :: HasTime t s => Wire s e IO a (Vector Float)
movPos = pure (^+>) <*> pure initPos <*> fmap (speed *) timeFloat

{-|
  The internal function for running the network.
-}
runNetwork' :: (HasTime t s, Renderable b) => IORef Bool -> Session IO s -> Wire s e IO a b -> IO ()
runNetwork' closedRef session wire = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      pollEvents

      (st, session') <- stepSession session
      (wt, wire'   ) <- stepWire wire st (Right undefined)

      case wt of
        Left  _     -> return ()
        Right state -> do
          clear [ColorBuffer]
          render state
          swapBuffers
          runNetwork' closedRef session' wire'

{-|
  Running the network.
-}
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef =
  runNetwork' closedRef clockSession_ movPos
