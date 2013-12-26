{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module SomeTypes where
import Language.Haskell.TH
import Language.Haskell.TH.Module.Magic
import Data.Maybe


data Test = Test Int

newtype OtherTest = OtherTest Test


someOtherFunction :: Bool -> Bool
someOtherFunction = \case
   True  -> False
   False -> True

names >>= runIO . print >> return []