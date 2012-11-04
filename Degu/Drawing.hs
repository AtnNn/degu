-- | Drawings

module Degu.Drawing where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Typeable
import System.Mem.Weak
import Control.Exception.Base

import Degu.DrawingUtils
import Degu.Render
import Degu.OpenGL

data Draw r rec where
     Draw :: forall a r rec . Typeable a => {
       bbox :: IfPrerendered r BBox,
       prerendered :: IfPrerendered r a,
       preRender :: PrerenderData -> IO (BBox, a),
       render :: BBox -> a -> IO ()
     } -> Draw r rec
     DrawGroup :: {
       precombine :: [BBox] -> BBox,
       combine :: [(BBox, IO ())] -> IO (),
       drawings :: [Draw r rec]
     } -> Draw r rec
     DrawOther :: rec -> Draw r rec

data BBox = BBox { bboxLeft, bboxBottom, bboxRight, bboxTop :: Int }

emptyBBox :: BBox
emptyBBox = BBox 0 0 0 0

newtype TextureObjectWrapper =
  TextureObjectWrapper GL.TextureObject deriving (Typeable)

mkDraw :: Typeable a
       => (PrerenderData -> IO (BBox, a))
       -> (BBox -> a -> IO ())
       -> Draw NotPrerendered rec
mkDraw =
  Draw (error "Void") (error "Void")


text :: String -> Int -> Draw NotPrerendered a
text str sz = mkDraw preRender display where

  preRender PrerenderData {..} = do
      (align, textSize@(GL.TextureSize2D w h), bitmap_) <- renderText sansSerif str sz
      tex <- evaluate =<< makeTexture textSize bitmap_
      addFinalizer tex $ GL.deleteObjectNames [tex]
      return (BBox 0 align (fromIntegral h) (fromIntegral w + align),
              TextureObjectWrapper tex)

  display (BBox l a r b) (TextureObjectWrapper tid) = do
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just tid
    GL.textureFunction $= GL.Replace
--    GL.color $ GL.Color4 0.8 0.9 0.8 (1 :: GL.GLfloat)
    let cv s t x y = GL.texCoord (GL.TexCoord2 (fromInteger s) (fromInteger t :: GL.GLfloat)) >>
                     GL.vertex (GL.Vertex2 (fromIntegral x) (fromIntegral y :: GL.GLfloat))
    GL.renderPrimitive GL.Quads $ do
      cv 1 0 l a
      cv 0 0 l b
      cv 0 1 r b
      cv 1 1 r a
    GL.texture GL.Texture2D $= GL.Disabled

prerenderDrawing :: PrerenderData -> Draw NotPrerendered rec -> IO (Draw Prerendered rec)
prerenderDrawing prdat Draw {..} = do
  (b, p) <- preRender prdat
  return $ Draw {bbox = b, prerendered = p, ..}
prerenderDrawing prdat DrawGroup {..} = do
  ds <- mapM (prerenderDrawing prdat) drawings
  return $ DrawGroup {drawings = ds, ..}
prerenderDrawing _prdat (DrawOther o) =
  return $ DrawOther o -- TODO
 
renderDrawing :: Draw Prerendered rec -> IO ()
renderDrawing Draw {render, bbox, prerendered} = render bbox prerendered
renderDrawing DrawGroup {combine, drawings} =
  combine $ map (\d -> (calculateBBox d, renderDrawing d)) drawings
renderDrawing (DrawOther _o) = return () -- TODO

calculateBBox :: Draw Prerendered rec -> BBox
calculateBBox Draw {bbox} = bbox
calculateBBox DrawGroup {precombine, drawings} = precombine $ map calculateBBox drawings
calculateBBox (DrawOther _o) = emptyBBox -- TODO

over :: Draw p rec -> Draw p rec -> Draw p rec
a `over` b = DrawGroup precombine combine [a,b] where
  precombine [BBox l b r t, BBox ll bb rr tt] =
    let c = (tt - b) `div` 2 in
      BBox (min l ll) (c + t - b) (max r rr) (c - tt + bb)
  combine [(BBox _ b _ _, top), (BBox _ _ _ tt, bottom)] =
    let c = (tt - b) `div` 2 in do
      GL.preservingMatrix $ GL.translate (GL.Vector3 0 (fromIntegral c) (0 :: GL.GLfloat)) >> top
      GL.preservingMatrix $ GL.translate (GL.Vector3 0 (fromIntegral (-c)) (0 :: GL.GLfloat)) >> bottom

overlap :: Draw p rec -> Draw p rec -> Draw p rec
a `overlap` b = DrawGroup precombine combine [a,b] where
  precombine [BBox l b r t, BBox ll bb rr tt] =
      BBox (min l ll) (min b bb) (max r rr) (max t tt)
  combine [(_, a), (_, b)] = a >> b


center :: Draw p rec -> Draw p rec
center = adjustBBox $ \(BBox l b r t) ->
  let r' = (r - l) `div` 2
      t' = (t - b) `div` 2 in
  BBox (r' - r + l) (t' - t + b) r' t'

adjustBBox :: (BBox -> BBox) -> Draw p rec -> Draw p rec
adjustBBox f d = DrawGroup precombine combine [d] where
  precombine [b] = f b
  combine [(bb@(BBox l b r t), m)] =
    case f bb of BBox ll bb rr tt -> GL.preservingMatrix $
                                    GL.translate (toVector3f (ll - l) (bb - b) 0) >> m

color :: GL.Color c => c -> Draw p rec -> Draw p rec
color c d = DrawGroup precombine combine [d] where
  precombine [b] = b
  combine [(_, m)] = GL.preservingAttrib [GL.ColorBufferAttributes] $
                     GL.color c >> m