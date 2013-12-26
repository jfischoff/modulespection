{-# LANGUAGE TemplateHaskell #-}
module SomeTypes where
import Language.Haskell.TH
import Language.Haskell.TH.Module.Magic
import Data.Maybe

data Test = Test Int

newtype OtherTest = OtherTest Test

someFunction :: String -> String
someFunction = id

names >>= runIO . print >> return []
--declarations >>= runIO . print >> return []
--reify 'someFunction >>= runIO . print >> return []

--moduleDeclarations "Data.Maybe" >>= runIO . print >> return []