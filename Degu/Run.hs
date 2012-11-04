module Degu.Run where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Degu.OpenGL
import Degu.Interface
import Degu.Font
import Degu.GlobalState
import Degu.Render

runInterface :: ((String, [String]) -> Interface) -> IO ()
runInterface ifaceGen = do
  (self, args) <- getArgsAndInitialize
  let iface = ifaceGen (self, args)
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, RGBAMode, WithAlphaComponent]
  _ <- createWindow "Degu"
  shadeModel $= Flat
  rowAlignment Unpack $= 1
  depthFunc $= Just Less
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  alphaFunc $= Just (Greater, 0.5)
  fontLib <- maybe (fail "Error initialising font library") return
             =<< initFont
  -- TODO: portable font path
  sansSerif <- maybe (fail "Error loading DejaVuSans.ttf") return
               =<< loadFace fontLib "/home/atnnn/code/degu/data/DejaVuSans.ttf"
  let prerenderData = (PrerenderData sansSerif)
  ifaceRef <- newIORef =<< prerenderInterface prerenderData iface
  st <- mkState ifaceRef prerenderData
  windowSize $= Size 1280 800
  idleCallback $= Just (idle st)
  displayCallback $= (display st)
  keyboardMouseCallback $= Just input
  reshapeCallback $= Just (reshape st)
  mainLoop

reshape :: State -> ReshapeCallback
reshape _st size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

display :: State -> IO ()
display st = do
  clearColor $= dark
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  Size w h <- fmap snd $ get viewport
  scale (1 / fromIntegral (w `div` 2)) (1 / fromIntegral (h `div` 2)) (0 :: GLfloat)
  preservingMatrix (draw st)
  swapBuffers

draw :: State -> IO ()
draw State {..} = do
  iface <- readIORef rootInterface
  preservingMatrix $ renderInterface iface

idle :: State -> IO ()
idle State {..} = do
  mod <- readIORef wasModified
  if mod then do
    iface <- readIORef rootInterface
    miface' <- tryTick iface
    case miface' of
      Nothing -> writeIORef wasModified False
      Just iface' -> do
        writeIORef rootInterface iface'
        postRedisplay Nothing
    else return ()

tryTick _ = return Nothing

input :: KeyboardMouseCallback
input key state modifiers position = do
  print (key, state, modifiers, position)
  return ()