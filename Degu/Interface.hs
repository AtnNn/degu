-- | Basic interface for Degu
-- 
-- An interface combines drawings and handlers
-- 
module Degu.Interface where

import Data.Typeable
import Control.Applicative
import Control.Monad.Trans

import Degu.Render
import Degu.Drawing

type Interface = Iface NotPrerendered
type PrerenderedInterface = Iface Prerendered

data Iface (r :: PrerenderState) = Interface {
  handlers :: [Handler],
  drawing :: Draw r Interface
  }

data Handler where Handler :: Typeable a => (a -> InterfaceM ()) -> Handler

newtype InterfaceM a = InterfaceM (IO a) deriving (Monad, Functor, Applicative, MonadIO)

prerenderInterface :: PrerenderData -> Interface -> IO PrerenderedInterface
prerenderInterface prdat Interface {drawing, ..} = do
  prDrawing <- prerenderDrawing prdat drawing
  return $ Interface { drawing = prDrawing, ..}
  
renderInterface :: PrerenderedInterface -> IO ()
renderInterface = renderDrawing . drawing