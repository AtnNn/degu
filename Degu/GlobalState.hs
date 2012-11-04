-- | The State type with some global objects

module Degu.GlobalState where

import Data.IORef

import Degu.Interface
import Degu.Render

data State = State {
  rootInterface :: IORef PrerenderedInterface,
  prerenderData :: PrerenderData,
  wasModified :: IORef Bool
  }

mkState :: (IORef PrerenderedInterface) -> PrerenderData -> IO State
mkState a b = fmap (State a b) $ newIORef True
