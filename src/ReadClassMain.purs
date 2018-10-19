module Test.ReadClassMain where

import Prelude

import Data.Binary.Binary (get)
import Data.Binary.Decoder (decodeFull)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, runAff, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import JVM.ClassFile (ClassFile(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff as ND
import Node.Buffer as ND
import Node.Path (concat)

main :: Effect Unit 
main = launchAff_ $ do
  buff <- ND.readFile $ concat [".", "test", "resources", "Class1.class"]
  ab <- liftEffect $ ND.toArrayBuffer buff
  (clss :: ClassFile) <- liftEffect $ decodeFull get ab 
  liftEffect $ log $ show clss