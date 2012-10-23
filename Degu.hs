module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Control.Applicative

data State = State {
  viewportSize :: IORef Size
  }

main :: IO ()
main = do
  (self, _) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, RGBMode]
  createWindow "Degu"
  depthFunc $= Just Less
  let vpSize = Size 500 500
  st <- mkState vpSize
  windowSize $= vpSize
  idleCallback $= Just idle
  displayCallback $= (display st)
  keyboardMouseCallback $= Just input
  reshapeCallback $= Just (reshape st)
  mainLoop

mkState :: Size -> IO State
mkState vpSize = State <$> newIORef vpSize

reshape :: State -> ReshapeCallback
reshape st size = do
  writeIORef (viewportSize st) size
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

display :: State -> IO ()
display st = do
  clearColor $= Color4 0 0.1 0.2 0.3
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  Size w h <- readIORef (viewportSize st)
  scale (2 / fromIntegral w) (2 / fromIntegral h) (0 :: GLfloat)
  preservingMatrix draw
  swapBuffers

draw :: IO ()
draw = do
  preservingMatrix points
  preservingMatrix text

text = do
  color $ Color3 1 1 (1 :: GLfloat)
  translate $ Vector3 (-400) (-50) (0 :: GLfloat)
  renderString Roman "> "
  color $ Color3 1 0 (0 :: GLfloat)

  
points = renderPrimitive Points $ mapM_ vertex $
       [Vertex3 (x * 20) (y * 20) 0 | x <- [-1, -0.9 .. 1],
                        y <- [-1, -0.9 .. 1 :: GLfloat]]

idle :: IO ()
idle = postRedisplay Nothing

input :: KeyboardMouseCallback
input key state modifiers position =
  return ()