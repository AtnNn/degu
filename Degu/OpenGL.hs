-- | OpenGL helper types
module Degu.OpenGL where

import Graphics.Rendering.OpenGL

type Vector3f = Vector3 GLfloat

type Color3f = Color3 GLfloat

toVector3f :: Int -> Int -> Int -> Vector3f
toVector3f a b c = Vector3 (fromIntegral a) (fromIntegral b) (fromIntegral c)

red :: Color3f
red = Color3 0.9 0.2 0.1

blue :: Color3f
blue = Color3 0.1 0.2 0.9

dark :: Fractional a => Color4 a
dark = Color4 0 0.1 0.2 0