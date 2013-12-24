{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE StandaloneDeriving        #-}
module Language.Haskell.TH.Module.Magic 
   ( declarations
   , getModuleDeclarations
   ) where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Applicative
import Data.List
import Control.Monad
import Data.Char
import Control.Arrow
import Data.Maybe
import qualified GHC
import Module
import GHC.Paths ( libdir )
import DynFlags 
import Name hiding (Name)
import NameSet
import HscTypes( tyThingParent_maybe )
import qualified RdrName as RdrName
import RdrName ( getGRE_NameQualifier_maybes )
import Debug.Trace 
import MonadUtils

traceMsg :: Show a => String -> a -> a
traceMsg msg x = trace (msg ++ show x) x

lookupModule :: GHC.GhcMonad m => String -> m Module
lookupModule mName = lookupModuleName (GHC.mkModuleName mName)

lookupModuleName :: GHC.GhcMonad m => ModuleName -> m Module
lookupModuleName mName = GHC.lookupModule mName Nothing

wantInterpretedModule :: GHC.GhcMonad m => String -> m Module
wantInterpretedModule str = wantInterpretedModuleName (GHC.mkModuleName str)

wantInterpretedModuleName :: GHC.GhcMonad m => ModuleName -> m Module
wantInterpretedModuleName modname = do
   modl <- lookupModuleName modname
   let str = moduleNameString modname
   dflags <- getDynFlags
   is_interpreted <- GHC.moduleIsInterpreted modl

   return modl

-- Get all the declarations of the current file that 
-- can be parsed by 
declarations :: Q [Dec]
declarations = getModuleDeclarations . loc_filename =<< location

getModuleDeclarations :: String -> Q [Dec]
getModuleDeclarations moduleStr = do
    names <- runIO $ getModuleNames moduleStr

    let maybeDec n = do
            info <- reify n
            case info of
                TyConI dec -> return $ Just dec
                _          -> return Nothing

    mapMaybeM maybeDec names

getModuleNames :: String -> IO [Name]
getModuleNames moduleStr = GHC.defaultErrorHandler defaultFatalMessager 
    defaultFlushOut $ do
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        GHC.setSessionDynFlags dflags
        md <- wantInterpretedModule moduleStr
        browseModule md

filterOutChildren :: (a -> GHC.TyThing) -> [a] -> [a]
filterOutChildren get_thing xs
  = filterOut has_parent xs
  where
    all_names = mkNameSet (map (GHC.getName . get_thing) xs)
    has_parent x = case tyThingParent_maybe (get_thing x) of
                     Just p  -> GHC.getName p `elemNameSet` all_names
                     Nothing -> False

filterOut :: (a -> Bool) -> [a] -> [a]
filterOut p xs = filter (not . p) xs

browseModule :: GHC.GhcMonad m => Module -> m [Name]
browseModule modl = do
  -- :browse reports qualifiers wrt current context
  unqual <- GHC.getPrintUnqual

  mb_mod_info <- GHC.getModuleInfo modl
  case mb_mod_info of
    Nothing -> error "browseModule"
    Just mod_info -> do
        dflags <- getDynFlags
        let names = GHC.modInfoExports mod_info

        mb_things <- mapM GHC.lookupName $ (\x -> trace (show $ length x) x) $ names
        let things = catMaybes $ (\x -> trace (show $ length x) x) mb_things
            --things = filterOutChildren (\t -> t) (catMaybes mb_things)

        rdr_env <- GHC.getGRE

        let modNames   = map GHC.getName things

            importInfo = RdrName.getGRE_NameQualifier_maybes rdr_env

        return $ map (mkName . occNameString . nameOccName) modNames

