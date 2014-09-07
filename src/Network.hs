module Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire
import FRP.Netwire
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
maxSpeed :: Float
maxSpeed = 1.0

{-|
  The speed in which the user accelerates.
-}
acceleration :: Float
acceleration = 2.0

{-|
  The initial position.
-}
initPos :: Vector Float
initPos = Vector 0.0 0.0

{-|
  Checking if a given key is down.
-}
isKeyDown :: (Enum k, Monoid s) => k -> Wire s () IO a ()
isKeyDown k =
  mkGen_ $ \_ -> do
    p <- getKey k
    case p of
      Press   -> return $ Right ()
      Release -> return $ Left ()

{-|
  The x-acceleration of the block.
-}
xAcceleration :: (HasTime t s, Monoid s) => Wire s () IO a (Vector Float)
xAcceleration  =  pure (leftDir  ^*> acceleration) . isKeyDown (CharKey 'A')
              <|> pure (rightDir ^*> acceleration) . isKeyDown (CharKey 'D')
              <|> pure (Vector 0 0)

{-|
  The y-acceleration of the block.
-}
yAcceleration :: Monoid s => Wire s () IO a (Vector Float)
yAcceleration  =  pure (upDir   ^*> acceleration) . isKeyDown (CharKey 'W')
              <|> pure (downDir ^*> acceleration) . isKeyDown (CharKey 'S')
              <|> pure (Vector 0 0)

{-|
  The velocity of the block.
-}
gameVelocity :: HasTime t s => Wire s () IO a (Vector Float)
gameVelocity  = liftA2 (^+) (vIntegral (pure 0) . xAcceleration)
                            (vIntegral (pure 0) . yAcceleration)

{-|
  The @'integral'@ but written on a Vector.
-}
vIntegral :: (Fractional a, HasTime t s) => Vector a -> Wire s e m (Vector a) (Vector a)
vIntegral v =
  mkPure $ \ds d ->
    let dt = realToFrac $ dtime ds in
      (Right v, vIntegral $ v ^+ (d ^*> dt))

{-|
  The game.
-}
game :: HasTime t s => Wire s () IO a (Vector Float)
game = vIntegral initPos . gameVelocity

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
  runNetwork' closedRef clockSession_ game
