module Degu.REPL where 

import Degu.Interface
import Degu.Drawing
import Degu.OpenGL

repl :: (String, [String]) -> Interface
repl (_self, _args) = Interface [] $ center $ 
  color red (text "Hello World," 200) `over`
  color blue (text "FooBar" 300)