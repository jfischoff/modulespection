{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP        #-}
module SomeTypes where
import Language.Haskell.TH
import Language.Haskell.TH.Module.Magic
import Data.Maybe
import Data.Aeson
import Data.Aeson.TH
import MonadUtils

#define ONE 1

data Test = Test Int

newtype OtherTest = OtherTest Test

someFunction :: String -> String
someFunction x = show ONE ++ x

someOtherFunction :: Bool -> Bool
someOtherFunction = \case
   True  -> False
   False -> True

-- names >>= runIO . print >> return [] 
--declarations >>= runIO . print >> return []
--reify 'someFunction >>= runIO . print >> return []

--moduleDeclarations "Data.Maybe" >>= runIO . print >> return []
moduleDeclarations "Data.Monoid" >>= concatMapM 
   (\(DataD _ n _ _ _) -> deriveJSON defaultOptions n)