module Demo where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..))
import Graphics.X11.Turtle (Turtle, pencolor, beginfill, endfill, speed, openField)
import Graphics.X11.Turtle qualified as Turtle
import Linear.Metric (norm)
import Linear.V2 (V2 (..))
import Relude hiding (break)

main :: IO ()
main = do
    f <- openField

    pauseSeconds 1

    runConcurrently $ foldMap Concurrently
      [ newTurtle f >>= \t -> star t 0 "red"
      , newTurtle f >>= \t -> star t 180 "blue"
      , newTurtle f >>= \t -> spiral t
      ]

star :: (MonadIO m, Turtle.ColorClass c) => Turtle -> Double -> c -> m ()
star t startAngle color = liftIO do
    speed t "fast"

    pencolor t color
    beginfill t

    left t startAngle

    whileTrue do
        forward t 200
        p <- position t
        when (norm p < 1) break
        left t 170
        pauseSeconds (2 / 10)

    endfill t

spiral :: MonadIO m => Turtle -> m ()
spiral t = liftIO do
    Turtle.pensize t 2

    angle <- newIORef 20

    whileTrue do
        forward t 15
        a <- readIORef angle
        left t a
        when (a < 1) break
        writeIORef angle (a * 0.99)

newTurtle :: MonadIO m => Turtle.Field -> m Turtle
newTurtle f = liftIO (Turtle.newTurtle f)

position :: MonadIO m => Turtle -> m (V2 Double)
position t = liftIO (Turtle.position t) <&> \(x, y) -> V2 x y

break :: Applicative m => MaybeT m a
break = MaybeT (pure Nothing)

forward :: MonadIO m => Turtle -> Double -> m ()
forward t x = liftIO (Turtle.forward t x)

left :: MonadIO m => Turtle -> Double -> m ()
left t x = liftIO (Turtle.left t x)

right :: MonadIO m => Turtle -> Double -> m ()
right t x = liftIO (Turtle.right t x)

whileTrue :: Monad m => MaybeT m a1 -> m ()
whileTrue a = void $ runMaybeT $ forever a

pauseSeconds :: MonadIO m => Rational -> m ()
pauseSeconds x = liftIO $ threadDelay $ round $ x * 1_000_000
