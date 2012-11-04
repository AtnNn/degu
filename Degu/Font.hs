module Degu.Font where

import Control.Monad
import Graphics.Rendering.FreeType.Internal as FT
import Graphics.Rendering.FreeType.Internal.Vector as V
import Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes as PT
import qualified Graphics.Rendering.FreeType.Internal.Face as F
import Graphics.Rendering.FreeType.Internal.Library as L
import Graphics.Rendering.FreeType.Internal.Bitmap as B
import Graphics.Rendering.FreeType.Internal.Size as S
import Graphics.Rendering.FreeType.Internal.SizeMetrics as SM

import Foreign.Concurrent
import Foreign hiding (newForeignPtr)
import Foreign.C.String

import Control.Applicative

newtype FontLib = FontLib (ForeignPtr FT_LibraryRec_)

initFont :: IO (Maybe FontLib)
initFont = do
  library_ <- alloca $ \libraryptr -> do
    err <- ft_Init_FreeType libraryptr
    if err /= 0
      then return Nothing
      else Just <$> peek (libraryptr :: Ptr FT_Library)
  flip (maybe (return Nothing)) library_ $ \l -> do
    lp <- newForeignPtr l ({-ft_Done_FreeType l >> -}return ())
    return $ Just $ FontLib lp

newtype FontFace = FontFace (ForeignPtr F.FT_FaceRec_)

loadFace :: FontLib -> FilePath -> IO (Maybe FontFace)
loadFace (FontLib lp) filename = 
  withForeignPtr lp $ \library_ ->
  alloca $ \faceptr -> do
    face_ <- withCString filename $ \str -> do
      err <- ft_New_Face library_ str 0 faceptr
      if err /= 0
        then return Nothing
        else Just <$> peek faceptr
    flip (maybe (return Nothing)) face_ $ \f -> do
      fp <- newForeignPtr f (ft_Done_Face f >> touchForeignPtr lp)
      return $ Just $ FontFace fp

setFaceSize :: FontFace -> Int -> IO (Maybe ())
setFaceSize (FontFace fp) h =
  withForeignPtr fp $ \face_ -> do
  err <- ft_Set_Pixel_Sizes face_ 0 (fromIntegral h)
  return $ if err /= 0 then Nothing else Just ()

faceMaxSize :: FontFace -> Int -> IO (Int, Int)
faceMaxSize face_ h = do
  (x, y, xx, yy) <- faceBBox face_ h
  return (xx - x, yy - y)

faceAlign :: FontFace -> Int -> IO Int
faceAlign face_ h = do
  (_, y, _, _) <- faceBBox face_ h
  return y

faceBBox :: FontFace -> Int -> IO (Int, Int, Int, Int)
faceBBox (FontFace fp) h = 
  withForeignPtr fp $ \face_ -> do
    _ <- setFaceSize (FontFace fp) h
    FT_Size_Metrics {..} <-
      peek . S.metrics =<< peek (F.size face_)
    let ht = fromIntegral height
        desc = fromIntegral descender
    return (0,
            desc `shiftR` 6,
            fromIntegral max_advance `shiftR` 6,
            (ht + desc) `shiftR` 6)
    {-
    (FT_BBox x y xx yy) <- peek $ bbox face
    upm <- fmap fromIntegral $ peek $ units_per_EM face
    let a n s p = fromInteger (((toInteger n * toInteger s * toInteger p) `div` upm) `shiftR` 16)
    return (a x x_scale x_ppem, a y y_scale y_ppem,
            a xx x_scale x_ppem, a yy y_scale y_ppem)
    -}

write :: FontFace -> Int -> String
      -> (Int -> (Int, Int) -> (Int, Int) -> FT_Bitmap -> IO (Maybe a))
      -> IO (Either Int a)
write (FontFace fp) h text f =  
  withForeignPtr fp $ \face_ -> do
  _ <- setFaceSize (FontFace fp) h
  slot <- peek $ F.glyph face_
  -- yd <- fmap ((`shiftR` 6) . negate . yMin) $ peek (bbox face)
  let
    go dx _ [] = return (Left dx)
    go dx n (c:cs) = do
      err <- ft_Load_Char face_ (fromIntegral . fromEnum $ c) ft_LOAD_RENDER
      if (err /= 0) then go dx (n+1) cs else do
      w <- fmap ((`shiftR` 6) . x) $ peek $ advance slot
      b <- peek $ bitmap slot
      -- TODO: ft_Get_Kerning
      left <- peek $ bitmap_left slot
      top  <- peek $ bitmap_top slot
      ma <- f n (dx + fromIntegral left, h - fromIntegral top)
              (fromIntegral $ width b, fromIntegral $ rows b) b
      maybe (go (dx + fromIntegral w) (n + 1) cs) (return . Right) ma
   in go 0 0 text 

test :: FilePath -> Int -> String -> IO ()
test path size_ text = do
  Just fontLib <- initFont
  Just fontFace <- loadFace fontLib path
  _ <- write fontFace size_ text go
  return ()
  where
    go _ (_x,_y) (w,h) bitmap_ = do
        forM_ [0 .. h - 1] $ \yy -> do
          forM_ [0 .. w - 1] $ \xx -> do
            x_ <- peek ((buffer bitmap_) `plusPtr` (fromIntegral $ yy * w + xx))
            putChar $ if x_ == (0 :: Word8) then '.' else if x_ < 250 then '*' else '#'
          putChar '\n'
        return Nothing
      

