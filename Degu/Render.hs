-- | Rendering drawings
-- 
-- First pre-render drawings
-- Then display them
-- 
module Degu.Render where

import Degu.Font

data PrerenderState = Prerendered | NotPrerendered

data Void

type family IfPrerendered (r :: PrerenderState) (a :: *) :: *
type instance IfPrerendered Prerendered a = a
type instance IfPrerendered NotPrerendered a = Void

data PrerenderData = PrerenderData {
  sansSerif :: FontFace
  }