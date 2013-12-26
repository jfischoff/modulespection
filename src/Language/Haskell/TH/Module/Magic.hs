{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.Haskell.TH.Module.Magic 
   ( -- * Name Introspection
     names
   , moduleNames
     -- * Declaration Introspection
   , declarations
   , moduleDeclarations
   ) where
import Language.Haskell.TH as TH
import Data.Maybe
import GHC
import Module
import GHC.Paths ( libdir )
import DynFlags 
import Name as Name
import RdrName 
import MonadUtils
import HsDecls as HsDecls
import SrcLoc
import Bag
import Control.Monad
import Data.Monoid

-- | Get all the top level declarations of the current file.
--   All names are returned whether they are exported or not.
names :: Q [TH.Name]
names = moduleNames . loc_filename =<< location

-- | Get all the top level names of a given module. 
--   If a file path is used, all names, exported and internal
--   are returned. If a module name is used, only the exported
--   names are returned.
moduleNames :: String -> Q [TH.Name]
moduleNames target = runIO $ 
   defaultErrorHandler 
      defaultFatalMessager 
      defaultFlushOut 
      $ do
         runGhc (Just libdir) $ do
           dflags <- getSessionDynFlags
           setSessionDynFlags dflags
           lookupModuleNames target

-- | Look up a name, and get out the declaration 
--   or return nothing
nameToMaybeDec :: TH.Name -> Q (Maybe Dec)
nameToMaybeDec name = do
   info <- reify name
   return $ case info of
      TyConI dec -> Just dec
      _          -> Nothing
      
-- | Get all the type declarations of the current file. 
--   Function and pattern declarations are ignored ... for now.
declarations :: Q [Dec]
declarations = mapMaybeM nameToMaybeDec =<< names

-- | Get all the top level names of a given module. 
--   If a file path is used, all names, exported and internal
--   are returned. If a module name is used, only the exported
--   names are returned.
--   Function and pattern declarations are ignored ... for now.
moduleDeclarations :: String -> Q [Dec]
moduleDeclarations = mapMaybeM nameToMaybeDec <=< moduleNames 

-- | Either try to parse a source file or if the module is
--   part of library, look it up and browse the contents
lookupModuleNames :: GhcMonad m => String -> m [TH.Name]
lookupModuleNames mName = do   
   target <- targetId <$> guessTarget mName Nothing
   case target of
      TargetModule moduleName -> getExistingModuleNames 
                             =<< lookupModule moduleName Nothing
      TargetFile filePath _   -> parseFile filePath

-- | Turn ErrorMessages into a String
errString :: Show a => Bag a -> String     
errString = unlines 
          . map show 
          . foldBag (<>) (:[]) []

-- | Parse a file and collect all of the declarations names
parseFile :: GhcMonad m => FilePath -> m [TH.Name]
parseFile filePath = do
   dflags <- getDynFlags
   src    <- liftIO $ readFile filePath 
   let (warns, L _ hsModule) = 
         either (error . errString) id
               $ parser src dflags filePath
                                 
       names = mapMaybe getNameMaybe $ hsmodDecls hsModule
       
   return $ map rdrNameToName names

showModuleName :: Module -> String
showModuleName = moduleNameString . moduleName

getExistingModuleNames :: GhcMonad m => Module -> m [TH.Name]
getExistingModuleNames modl = do
  moduleInfo <- getModuleInfo modl
  case moduleInfo of
    Nothing -> error $ "modulespection: Failed to find module info for "
                     <> showModuleName modl
                     <> " in getExistingModuleNames"
    Just mod_info -> fmap (map (occNameToName . nameOccName . getName))
                  .  mapMaybeM lookupName 
                  $  modInfoExports mod_info

-- | Simple Class for getting the name of things
class GetNameMaybe a where
   getNameMaybe :: a -> Maybe RdrName

instance GetNameMaybe (HsDecl RdrName) where
   getNameMaybe = \case 
      TyClD x -> getNameMaybe x
      HsDecls.ValD  x -> getNameMaybe x
      _           -> Nothing

instance GetNameMaybe (TyClDecl RdrName) where
   getNameMaybe = \case
      ForeignType x _   -> getNameMaybe x
      x@(TyFamily   {}) -> getNameMaybe $ tcdLName x
      TyDecl    x _ _ _ -> getNameMaybe x
      x@(ClassDecl {})  -> getNameMaybe $ tcdLName x

instance GetNameMaybe (HsBindLR RdrName RdrName) where
   getNameMaybe = \case 
      x@(FunBind {}) -> getNameMaybe $ fun_id x 
      _                  -> Nothing

instance GetNameMaybe a => GetNameMaybe (GenLocated SrcSpan a) where
   getNameMaybe (L _ x) = getNameMaybe x 

instance GetNameMaybe RdrName where
   getNameMaybe = Just

-- Name Utils
occNameToName :: OccName -> TH.Name
occNameToName = mkName . occNameString 

rdrNameToName :: RdrName -> TH.Name
rdrNameToName = \case 
   RdrName.Unqual x -> occNameToName x
   RdrName.Qual _ x -> occNameToName x
   RdrName.Orig _ x -> occNameToName x
   RdrName.Exact  x -> occNameToName $ nameOccName x