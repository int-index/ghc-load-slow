module Main (main) where

import Data.Foldable (toList)
import System.Environment.Blank (getEnv)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Scientific (Scientific)
import qualified Data.Text as Text
import Data.Ratio
import System.CPUTime
import Data.Fixed
import Data.Foldable (for_)

import qualified GHC
import qualified GHC.Data.Bag as GHC
import qualified GHC.Types.Name as GHC
import qualified GHC.Types.SourceText as GHC
import qualified GHC.Types.PkgQual as GHC
import qualified GHC.Data.FastString as GHC
import qualified GHC.Types.Basic as GHC
import qualified GHC.Utils.Lexeme as GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Utils.Outputable as GHC
import qualified GHC.LanguageExtensions as GHC.LangExt

main :: IO ()
main = do
  -- Print what we compile:
  for_ ghcDecls \ghcDecl -> putStrLn $ GHC.showPprUnsafe ghcDecl
  putStrLn $ GHC.showPprUnsafe ghcExpr
  -- Compile and measure:
  pkgPaths <- getPkgPaths
  putStrLn "First run:"
  compileApp pkgPaths
  putStrLn "\nSecond run:"
  compileApp pkgPaths

compileApp :: PkgPaths -> IO ()
compileApp pkgPaths = do
  p0 <- getCPUTime
  ghcLoadApp pkgPaths
  p1 <- getCPUTime
  putStrLn ("ghcLoadApp: " ++ show (MkFixed (p1 - p0) :: Pico))

data PkgPaths =
  PkgPaths {
    libDir :: FilePath,
    pkgDb :: Maybe FilePath
  }

getLibDir :: IO FilePath
getLibDir = do
  mLibDir <- getEnv "GLS_LIBDIR"
  case mLibDir of
    Just libDir -> return libDir
    Nothing -> do
      hPutStrLn stderr "GLS_LIBDIR is unset"
      exitFailure

getPkgDb :: IO (Maybe FilePath)
getPkgDb = getEnv "GLS_PKGDB"

getPkgPaths :: IO PkgPaths
getPkgPaths = do
  libDir <- getLibDir
  pkgDb <- getPkgDb
  return PkgPaths{libDir, pkgDb}

ghcLoadApp :: PkgPaths -> IO ()
ghcLoadApp pkgPaths = do
  p0 <- getCPUTime
  GHC.runGhc (Just pkgPaths.libDir) do
    p1 <- liftIO $ getCPUTime
    liftIO $ putStrLn ("Set up session: " ++ show (MkFixed (p1 - p0) :: Pico))
    initial_dflags <- GHC.getSessionDynFlags
    GHC.setSessionDynFlags (modify_dflags initial_dflags)
    GHC.setContext [GHC.IIDecl importGLSPlatform]
    p2 <- liftIO $ getCPUTime
    liftIO $ putStrLn ("Set context: " ++ show (MkFixed (p2 - p1) :: Pico))
    GHC.runParsedDecls ghcDecls
    p3 <- liftIO $ getCPUTime
    liftIO $ putStrLn ("Run decls: " ++ show (MkFixed (p3 - p2) :: Pico))
    GHC.compileParsedExpr ghcExpr
    p4 <- liftIO $ getCPUTime
    liftIO $ putStrLn ("Compile expr: " ++ show (MkFixed (p4 - p3) :: Pico))
  where
    modify_dflags :: GHC.DynFlags -> GHC.DynFlags
    modify_dflags dflags =
      dflags
        { GHC.backend = GHC.ncgBackend,
          GHC.ghcLink = GHC.LinkBinary,
          GHC.packageDBFlags = [
              -- 1. Install the platform in an isolated dir: CABAL_DIR="$(pwd)/platform-cabal/" cabal install --lib gls-platform
              -- 2. Use GLS_PKGDB="$(pwd)/platform-cabal/store/ghc-9.4.4/package.db" (or whatever is the GHC version)
              GHC.PackageDB (GHC.PkgDbPath pkgDbPath) | pkgDbPath <- toList pkgPaths.pkgDb
          ] ++ GHC.packageDBFlags dflags,
          GHC.packageFlags = [
              GHC.ExposePackage "-package gls-platform" (GHC.PackageArg "gls-platform") (GHC.ModRenaming True [])
            ] ++ GHC.packageFlags dflags
        }
      `GHC.xopt_set` GHC.LangExt.PackageImports

ghcDecls :: [GHC.LHsDecl GHC.GhcPs]
ghcDecls =
  [tgDataDecl
     (tgUnqual CtxType "AppState0")
     (tgUnqual CtxTerm "MkAppState1")
     [tgTyVar (tgQual (GHC.mkTcOcc "Text")),
      tgTyVar (tgQual (GHC.mkTcOcc "Int"))]]

ghcExpr :: GHC.LHsExpr GHC.GhcPs
ghcExpr =
  tgFunApp
     (tgVar (tgQual (GHC.mkDataOcc "SomeApp")))
     (tgFunApp
        (tgFunApp
           (tgVar (tgQual (GHC.mkDataOcc "App")))
           (tgFunApp
              (tgVar (tgQual (GHC.mkVarOcc "return")))
              (tgLetRec
                 [(GHC.nlVarPat (tgUnqual CtxTerm "s2"), tgStrLit "Hello,\nWorld"),
                  (GHC.nlVarPat (tgUnqual CtxTerm "i3"), tgNumLit 8.0)]
                 (tgFunApp
                    (tgFunApp
                       (tgVar (tgUnqual CtxTerm "MkAppState1"))
                       (tgVar (tgUnqual CtxTerm "s2")))
                    (tgVar (tgUnqual CtxTerm "i3"))))))
        (GHC.mkHsLam
           [GHC.nlConPat
              (tgUnqual CtxTerm "MkAppState1")
              [GHC.nlVarPat (tgUnqual CtxTerm "s4"),
               GHC.nlVarPat (tgUnqual CtxTerm "i5")],
            GHC.nlVarPat (tgUnqual CtxTerm "ctx")]
           (tgVar
              (tgQual (GHC.mkVarOcc "layoutText")))))

glsPlatformModuleName :: GHC.ModuleName
glsPlatformModuleName = GHC.mkModuleName "GLS_Platform"

glsPlatformModuleAlias :: GHC.ModuleName
glsPlatformModuleAlias = GHC.mkModuleName "GLS"

importGLSPlatform :: GHC.ImportDecl GHC.GhcPs
importGLSPlatform =
  GHC.ImportDecl {
      GHC.ideclExt       = GHC.XImportDeclPass GHC.noAnn GHC.NoSourceText False,
      GHC.ideclName      = GHC.noLocA glsPlatformModuleName,
      GHC.ideclPkgQual   = GHC.RawPkgQual (GHC.StringLiteral GHC.NoSourceText "gls-platform" Nothing),
      GHC.ideclSource    = GHC.NotBoot,
      GHC.ideclSafe      = False,
      GHC.ideclQualified = GHC.QualifiedPre,
      GHC.ideclAs        = Just $ GHC.noLocA glsPlatformModuleAlias,
      GHC.ideclImportList = Nothing
    }

tgDataDecl :: GHC.RdrName -> GHC.RdrName -> [GHC.LHsType GHC.GhcPs] -> GHC.LHsDecl GHC.GhcPs
tgDataDecl tyName conName fieldTys =
  GHC.noLocA $ GHC.TyClD GHC.noExtField $
    GHC.DataDecl {
      GHC.tcdDExt = GHC.noAnn,
      GHC.tcdLName = GHC.noLocA tyName,
      GHC.tcdTyVars = GHC.HsQTvs GHC.noExtField [],
      GHC.tcdFixity = GHC.Prefix,
      GHC.tcdDataDefn =
        GHC.HsDataDefn {
          GHC.dd_ext = GHC.noExtField,
          GHC.dd_cType = Nothing,
          GHC.dd_ctxt = Nothing,
          GHC.dd_kindSig = Nothing,
          GHC.dd_derivs = [],
          GHC.dd_cons =
            GHC.DataTypeCons False [GHC.noLocA $ GHC.ConDeclH98 {
              GHC.con_ext = GHC.noAnn,
              GHC.con_name = GHC.noLocA conName,
              GHC.con_forall = False,
              GHC.con_ex_tvs = [],
              GHC.con_mb_cxt = Nothing,
              GHC.con_args   = GHC.PrefixCon [] (map GHC.hsUnrestricted fieldTys),
              GHC.con_doc    = Nothing
            }]
        }
    }

tgTyVar :: GHC.RdrName -> GHC.LHsType GHC.GhcPs
tgTyVar name =
  GHC.noLocA $ GHC.HsTyVar GHC.noAnn GHC.NotPromoted $
  GHC.noLocA name

tgVar :: GHC.RdrName -> GHC.LHsExpr GHC.GhcPs
tgVar name =
  GHC.noLocA $ GHC.HsVar GHC.noExtField $
  GHC.noLocA name

tgStrLit :: Text -> GHC.LHsExpr GHC.GhcPs
tgStrLit str =
  let str' = GHC.fsLit (Text.unpack str) in
  GHC.noLocA $ GHC.HsOverLit GHC.noAnn (GHC.mkHsIsString GHC.NoSourceText str')

tgNumLit :: Scientific -> GHC.LHsExpr GHC.GhcPs
tgNumLit num =
  let num' = toRational num in
  GHC.noLocA $ GHC.HsOverLit GHC.noAnn $
    if denominator num' == 1
    then GHC.mkHsIntegral (GHC.mkIntegralLit (numerator num'))
    else GHC.mkHsFractional (GHC.mkTHFractionalLit (toRational num))

tgFunApp :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
tgFunApp fn arg =
  GHC.mkHsApp
    (GHC.parenthesizeHsExpr GHC.funPrec fn)
    (GHC.parenthesizeHsExpr GHC.appPrec arg)

tgLetRec :: [(GHC.LPat GHC.GhcPs, GHC.LHsExpr GHC.GhcPs)] -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
tgLetRec bindings e =
  GHC.noLocA $ GHC.HsLet GHC.noAnn GHC.noHsTok (toGhcBindings bindings) GHC.noHsTok e

toGhcBindings :: [(GHC.LPat GHC.GhcPs, GHC.LHsExpr GHC.GhcPs)] -> GHC.HsLocalBinds GHC.GhcPs
toGhcBindings bindings =
  GHC.HsValBinds GHC.noAnn $
  GHC.ValBinds
    GHC.NoAnnSortKey
    (GHC.listToBag [toGhcBinding lhs rhs | (lhs, rhs) <- bindings])
    []

toGhcBinding :: GHC.LPat GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs -> GHC.LHsBindLR GHC.GhcPs GHC.GhcPs
toGhcBinding lhs rhs =
  GHC.noLocA $
    GHC.PatBind GHC.noAnn lhs (mkGRHSs rhs)
  where
    mkGRHSs :: GHC.LHsExpr GHC.GhcPs -> GHC.GRHSs GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
    mkGRHSs e = (GHC.GRHSs GHC.emptyComments [GHC.noLocA $ GHC.GRHS GHC.noAnn [] e] GHC.emptyLocalBinds)

data NameCtx = CtxType | CtxTerm

tgQual :: GHC.OccName -> GHC.RdrName
tgQual = GHC.Qual glsPlatformModuleAlias

tgUnqual :: NameCtx -> Text -> GHC.RdrName
tgUnqual nctx str' =
  GHC.Unqual occName
  where
    str = Text.unpack str'
    occName =
      case nctx of
        CtxType -> if GHC.okVarOcc str then GHC.mkTyVarOcc str else GHC.mkTcOcc str
        CtxTerm -> if GHC.okVarOcc str then GHC.mkVarOcc str else GHC.mkDataOcc str

