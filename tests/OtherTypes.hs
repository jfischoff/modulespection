{-# LANGUAGE TemplateHaskell #-}
module OtherTypes where
import Language.Haskell.TH
import Language.Haskell.TH.Module.Magic
import Data.Monoid
import SomeTypes

$(do  
     dec <- getModuleDeclarations "SomeTypes"
     runIO $ print dec

     return [])
