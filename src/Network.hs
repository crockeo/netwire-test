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
speed = 0.5

{-|
  The initial position.
-}
initPos :: Vector Float
initPos = Vector 0 0

{-|
  Making the stateful position.
-}
watPos :: Vector Float -> Wire (Timed NominalDiffTime ()) e IO a (Vector Float)
watPos v =
  mkGen $ \(Timed t ()) _ -> do
    goup    <- fmap (== Press) $ getKey $ CharKey 'W'
    godown  <- fmap (== Press) $ getKey $ CharKey 'S'
    goleft  <- fmap (== Press) $ getKey $ CharKey 'A'
    goright <- fmap (== Press) $ getKey $ CharKey 'D'

    let v' = flatten (pure 0) [ (goup   , upDir   )
                              , (godown , downDir )
                              , (goleft , leftDir )
                              , (goright, rightDir)
                              ]
        v'' = v ^+ v' ^*> speed ^*> realToFrac t

    return (Right v'', watPos v'')
  where flatten :: Num a => Vector a -> [(Bool, Vector a)] -> Vector a
        flatten v []              = v
        flatten v ((False, _):xs) = flatten v xs
        flatten v ((True , d):xs) = flatten (v ^+ d) xs

{-|
  The internal function for running the network.
-}
runNetwork' :: Renderable b => IORef Bool -> Session IO (Timed NominalDiffTime ()) -> Wire (Timed NominalDiffTime ()) e IO a b -> IO ()
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
  runNetwork' closedRef clockSession_ $ watPos $ pure 0
