-- | Helper functions for making openGL Drawings

module Degu.DrawingUtils where 

import Degu.Font
import Graphics.Rendering.OpenGL
import Foreign
import Control.Monad
import Graphics.Rendering.FreeType.Internal.Bitmap

makeTexture :: TextureSize2D -> ForeignPtr GLbyte -> IO TextureObject
makeTexture sz@(TextureSize2D _w _h) fbm = do
  [tid] <- genObjectNames 1
  textureBinding Texture2D $= Just tid
  withForeignPtr fbm $ \bm -> do
    texImage2D Nothing NoProxy 0 Alpha' sz 0 (PixelData Alpha UnsignedByte bm)
    -- dump (fromIntegral w) (fromIntegral h) bm
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D S $= (Repeated, Clamp)
  textureWrapMode Texture2D T $= (Repeated, Clamp)
  return tid
  
  where _dump w_ h_ p =
          forM_ [0 .. w_ - 1] $ \x -> do
            forM_ [0 .. h_ - 1] $ \y ->
              putChar . _nice =<< peek (p `plusPtr` (x + y * w_))
            putStr "$\n"
        _nice :: Word8 -> Char
        _nice 0 = ' '
        _nice x | x < 64  = '.'
               | x < 128 = ','
               | x < 196 = ':'
               | x < 255 = '='
               | otherwise = '#'

renderText :: FontFace -> String -> Int -> IO (Int, TextureSize2D, ForeignPtr GLbyte)
renderText face text sz = do
  (bbw, bbh) <- faceMaxSize face sz
  align <- faceAlign face sz
  let bw = bbw * length text; bh = bbh
  fbm <- mallocForeignPtrBytes (bw * bh)
  bw' <- fmap (either id id) $ withForeignPtr fbm $ \bm -> do
    write face sz text $ \_ (x,y) (w,h) bitmap_ ->
      if x + w > bw then return $ Just x else do
      forM_ [0 .. w - 1] $ \xx -> do
        forM_ [0 .. h - 1] $ \yy -> do
          px <- peek ((buffer bitmap_) `plusPtr` (fromIntegral $ yy * w + xx))
          poke (bm `plusPtr` (y + yy + ((x + xx) * bh))) (px :: Word8)
      return Nothing
  return (align, TextureSize2D (fromIntegral bh) (fromIntegral bw'), fbm)

      