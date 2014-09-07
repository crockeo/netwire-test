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
speed :: Float
speed = 0.5

{-|
  The initial position.
-}
initPos :: Vector Float
initPos = Vector 0.5 0

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
  The x-velocity of the block.
-}
xGameVelocity :: Monoid s => Wire s () IO a (Vector Float)
xGameVelocity  =  pure (Vector 0 0)         . isKeyDown (CharKey 'A') . isKeyDown (CharKey 'D')
              <|> pure (leftDir  ^*> speed) . isKeyDown (CharKey 'A')
              <|> pure (rightDir ^*> speed) . isKeyDown (CharKey 'D')
              <|> pure (Vector 0 0)

{-|
  The y-velocity of the block.
-}
yGameVelocity :: Monoid s => Wire s () IO a (Vector Float)
yGameVelocity  =  pure (Vector 0 0)        . isKeyDown (CharKey 'W') . isKeyDown (CharKey 'S')
              <|> pure (upDir   ^*> speed) . isKeyDown (CharKey 'W')
              <|> pure (downDir ^*> speed) . isKeyDown (CharKey 'S')
              <|> pure (Vector 0 0)

{-|
  The velocity of the block.
-}
gameVelocity :: HasTime t s => Wire s () IO a (Vector Float)
gameVelocity  = liftA2 (^+) xGameVelocity
                            yGameVelocity

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
