{-# OPTIONS -fprof-late -Wno-unused-imports -Wno-name-shadowing #-}

{-# LANGUAGE
  NoImplicitPrelude
  ,BangPatterns
  ,ScopedTypeVariables
  ,MonoLocalBinds
  ,TypeOperators
  ,BangPatterns
  ,LambdaCase
  ,NondecreasingIndentation
  ,GADTs
  ,NamedFieldPuns
  ,TypeApplications
  ,MultiWayIf
  ,RecordWildCards
  #-}

module CopyGHC where

import GHC.Prelude

import GHC.Driver.Monad
import GHC.Driver.Errors.Types ( hoistTcRnMessage )
import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Config

import GHC.Runtime.Eval.Types
import GHC.Runtime.Interpreter as GHCi
import GHC.Runtime.Heap.Inspect
import GHC.Runtime.Context
import GHCi.Message
import GHCi.RemoteTypes
import GHC.ByteCode.Types

import GHC.Linker.Loader as Loader hiding (loadDecls)

import GHC.Hs

import GHC.Core.Predicate
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv ( FamInst, orphNamesOfFamInst )
import GHC.Core.TyCon
import GHC.Core.Type       hiding( typeKind )
import GHC.Core.TyCo.Ppr
import qualified GHC.Core.Type as Type

import GHC.Iface.Env       ( newInteractiveBinder )
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin

import GHC.Builtin.Names ( toDynName, pretendNameIsInScope )

import GHC.Data.Maybe
import GHC.Data.FastString
import GHC.Data.Bag

import GHC.Utils.Monad
import GHC.Utils.Panic
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Logger

import GHC.Types.RepType
import GHC.Types.Fixity.Env
import GHC.Types.Var
import GHC.Types.Id as Id
import GHC.Types.Name      hiding ( varName )
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Types.Var.Env
import GHC.Types.SrcLoc
import GHC.Types.Unique
import GHC.Types.Unique.Supply
import GHC.Types.Unique.DSet
import GHC.Types.TyThing
import GHC.Types.BreakInfo
import GHC.Types.Unique.Map

import GHC.Unit
import GHC.Unit.Module.Graph
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModSummary
import GHC.Unit.Home.ModInfo

import System.Directory
import Data.Dynamic
import Data.Either
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (find,intercalate)
import Data.List.NonEmpty (NonEmpty)
import Control.Monad
import Control.Monad.Catch as MC
import Data.Array
import GHC.Utils.Exception
import Unsafe.Coerce ( unsafeCoerce )

import GHC.Tc.Module ( runTcInteractive, tcRnType, loadUnqualIfaces )
import GHC.Tc.Utils.Zonk ( ZonkFlexi (SkolemiseFlexi) )
import GHC.Tc.Utils.Env (tcGetInstEnvs)
import GHC.Tc.Utils.Instantiate (instDFunType)
import GHC.Tc.Solver (simplifyWantedsTcM)
import GHC.Tc.Utils.Monad
import GHC.Core.Class (classTyCon)
import GHC.Unit.Env
import GHC.IfaceToCore
import GHC.Prelude

import GHC.Platform
import GHC.Platform.Ways

import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Env
import GHC.Driver.Env.KnotVars
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.CodeOutput
import GHC.Driver.Config.Cmm.Parser (initCmmParserConfig)
import GHC.Driver.Config.Core.Opt.Simplify ( initSimplifyExprOpts )
import GHC.Driver.Config.Core.Lint ( endPassHscEnvIO )
import GHC.Driver.Config.Core.Lint.Interactive ( lintInteractiveExpr )
import GHC.Driver.Config.CoreToStg
import GHC.Driver.Config.CoreToStg.Prep
import GHC.Driver.Config.Logger   (initLogFlags)
import GHC.Driver.Config.Parser   (initParserOpts)
import GHC.Driver.Config.Stg.Ppr  (initStgPprOpts)
import GHC.Driver.Config.Stg.Pipeline (initStgPipelineOpts)
import GHC.Driver.Config.StgToCmm  (initStgToCmmConfig)
import GHC.Driver.Config.Cmm       (initCmmConfig)
import GHC.Driver.LlvmConfigCache  (initLlvmConfigCache)
import GHC.Driver.Config.StgToJS  (initStgToJSConfig)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Tidy
import GHC.Driver.Hooks
import GHC.Driver.GenerateCgIPEStub (generateCgIPEStub)

import GHC.Runtime.Context
import GHC.Runtime.Interpreter ( addSptEntry )
import GHC.Runtime.Loader      ( initializePlugins )
import GHCi.RemoteTypes        ( ForeignHValue )
import GHC.ByteCode.Types
import GHC.Linker.Types

import GHC.Hs
import GHC.Hs.Dump
import GHC.Hs.Stats         ( ppSourceStats )

import GHC.HsToCore

import GHC.StgToByteCode    ( byteCodeGen )
import GHC.StgToJS          ( stgToJS )

import GHC.IfaceToCore  ( typecheckIface, typecheckWholeCoreBindings )

import GHC.Iface.Load   ( ifaceStats, writeIface )
import GHC.Iface.Make
import GHC.Iface.Recomp
import GHC.Iface.Tidy
import GHC.Iface.Ext.Ast    ( mkHieFile )
import GHC.Iface.Ext.Types  ( getAsts, hie_asts, hie_module )
import GHC.Iface.Ext.Binary ( readHieFile, writeHieFile , hie_file_result)
import GHC.Iface.Ext.Debug  ( diffFile, validateScopes )

import GHC.Core
import GHC.Core.Lint.Interactive ( interactiveInScope )
import GHC.Core.Tidy           ( tidyExpr )
import GHC.Core.Type           ( Type, Kind )
import GHC.Core.Multiplicity
import GHC.Core.Utils          ( exprType )
import GHC.Core.ConLike
import GHC.Core.Opt.Pipeline hiding (simplifyExpr)
import GHC.Core.Opt.Pipeline.Types      ( CoreToDo (..))
import GHC.Core.TyCon
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import GHC.Core.Rules
import GHC.Core.Stats
import GHC.Core.LateCC (addLateCostCentresPgm)


import GHC.CoreToStg.Prep
import GHC.CoreToStg    ( coreToStg )

import GHC.Parser.Errors.Types
import GHC.Parser
import GHC.Parser.Lexer as Lexer

import GHC.Tc.Module
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Zonk    ( ZonkFlexi (DefaultFlexi) )

import GHC.Stg.Syntax
import GHC.Stg.Pipeline ( stg2stg, StgCgInfos )

import GHC.Builtin.Utils
import GHC.Builtin.Names
import GHC.Builtin.Uniques ( mkPseudoUniqueE )

import qualified GHC.StgToCmm as StgToCmm ( codeGen )
import GHC.StgToCmm.Types (CmmCgInfos (..), ModuleLFInfos)

import GHC.Cmm
import GHC.Cmm.Info.Build
import GHC.Cmm.Pipeline
import GHC.Cmm.Info
import GHC.Cmm.Parser

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.External
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Graph
import GHC.Unit.Module.Imported
import GHC.Unit.Module.Deps
import GHC.Unit.Module.Status
import GHC.Unit.Home.ModInfo

import GHC.Types.Id
import GHC.Types.SourceError
import GHC.Types.SafeHaskell
import GHC.Types.ForeignStubs
import GHC.Types.Var.Env       ( emptyTidyEnv )
import GHC.Types.Error
import GHC.Types.Fixity.Env
import GHC.Types.CostCentre
import GHC.Types.IPE
import GHC.Types.SourceFile
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Cache ( initNameCache )
import GHC.Types.Name.Reader
import GHC.Types.Name.Ppr
import GHC.Types.Name.Set (NonCaffySet)
import GHC.Types.TyThing
import GHC.Types.HpcInfo

import GHC.Utils.Fingerprint ( Fingerprint )
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Logger
import GHC.Utils.TmpFs

import GHC.Data.FastString
import GHC.Data.Bag
import GHC.Data.StringBuffer
import qualified GHC.Data.Stream as Stream
import GHC.Data.Stream (Stream)
import GHC.Data.Maybe

import qualified GHC.SysTools
import GHC.SysTools (initSysTools)
import GHC.SysTools.BaseDir (findTopDir)

import Data.Data hiding (Fixity, TyCon)
import Data.List        ( nub, isPrefixOf, partition )
import qualified Data.List.NonEmpty as NE
import Control.Monad
import Data.IORef
import System.FilePath as FilePath
import System.Directory
import qualified Data.Set as S
import Data.Set (Set)
import Data.Functor
-- import Control.DeepSeq (force)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Unit.Module.WholeCoreBindings
import GHC.Types.TypeEnv
import System.IO
import GHC.Driver.Pipeline
-- import Data.Time

import System.IO.Unsafe ( unsafeInterleaveIO )
import GHC.Iface.Env ( trace_if )
import GHC.Stg.InferTags.TagSig (seqTagSig)
import GHC.Types.Unique.FM

import GHC.Core
import GHC.Core.Rules
import GHC.Core.Ppr     ( pprCoreBindings, pprCoreExpr )
import GHC.Core.Opt.OccurAnal ( occurAnalysePgm, occurAnalyseExpr )
import GHC.Core.Stats   ( coreBindsSize, coreBindsStats, exprSize )
import GHC.Core.Utils   ( mkTicks, stripTicksTop )
import GHC.Core.Lint    ( LintPassResultConfig, dumpPassResult, lintPassResult )
import GHC.Core.Opt.Simplify.Iteration ( simplTopBinds, simplExpr, simplImpRules )
import GHC.Core.Opt.Simplify.Utils ( activeRule, activeUnfolding )
import GHC.Core.Opt.Simplify.Env
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Opt.Stats ( simplCountN )
import GHC.Core.FamInstEnv

import GHC.Utils.Error  ( withTiming )
import GHC.Utils.Logger as Logger
import GHC.Utils.Outputable
import GHC.Utils.Constants (debugIsOn)

import GHC.Unit.Env ( UnitEnv, ueEPS )
import GHC.Unit.External
import GHC.Unit.Module.ModGuts

import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Basic
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Tickish
import GHC.Types.Unique.FM

import Control.Monad
import Data.Foldable ( for_ )



import GHC.Runtime.Eval hiding (runParsedDecls, compileParsedExpr, compileParsedExprRemote)
import GHC.Driver.Main hiding (hscParsedStmt, hscCompileCoreExpr, hscCompileCoreExpr')
import GHC.Core.Opt.Simplify hiding (simplifyExpr)
import CopyGHCLinker

-- | Like `runDeclsWithLocation`, but takes parsed declarations as argument.
-- Useful when doing preprocessing on the AST before execution, e.g. in GHCi
-- (see GHCi.UI.runStmt).
runParsedDecls :: GhcMonad m => [LHsDecl GhcPs] -> m [Name]
runParsedDecls decls = do
    hsc_env <- getSession
    (tyThings, ic) <- liftIO (hscParsedDecls hsc_env decls)

    setSession $ hsc_env { hsc_IC = ic }
    hsc_env <- getSession
    hsc_env' <- liftIO $ rttiEnvironment hsc_env
    setSession hsc_env'
    return $ filter (not . isDerivedOccName . nameOccName)
             -- For this filter, see Note [What to show to users]
           $ map getName tyThings

hscParsedStmt :: HscEnv
              -> GhciLStmt GhcPs  -- ^ The parsed statement
              -> IO ( Maybe ([Id]
                    , ForeignHValue {- IO [HValue] -}
                    , FixityEnv))
hscParsedStmt hsc_env stmt = runInteractiveHsc hsc_env $ do
  -- Rename and typecheck it
  (ids, tc_expr, fix_env) <- ioMsgMaybe $ hoistTcRnMessage $ tcRnStmt hsc_env stmt

  -- Desugar it
  ds_expr <- ioMsgMaybe $ hoistDsMessage $ deSugarExpr hsc_env tc_expr
  liftIO (lintInteractiveExpr (text "desugar expression") hsc_env ds_expr)
  handleWarnings

  -- Then code-gen, and link it
  -- It's important NOT to have package 'interactive' as thisUnitId
  -- for linking, else we try to link 'main' and can't find it.
  -- Whereas the linker already knows to ignore 'interactive'
  let src_span = srcLocSpan interactiveSrcLoc
  (hval,_,_) <- liftIO $ hscCompileCoreExpr hsc_env src_span ds_expr

  return $ Just (ids, hval, fix_env)

hscCompileCoreExpr :: HscEnv -> SrcSpan -> CoreExpr -> IO (ForeignHValue, [Linkable], PkgsLoaded)
hscCompileCoreExpr hsc_env loc expr =
  case hscCompileCoreExprHook (hsc_hooks hsc_env) of
      Nothing -> hscCompileCoreExpr' hsc_env loc expr
      Just h  -> h                   hsc_env loc expr

hscCompileCoreExpr' :: HscEnv -> SrcSpan -> CoreExpr -> IO (ForeignHValue, [Linkable], PkgsLoaded)
hscCompileCoreExpr' hsc_env srcspan ds_expr
    = do { {- Simplify it -}
           -- Question: should we call SimpleOpt.simpleOptExpr here instead?
           -- It is, well, simpler, and does less inlining etc.
           let dflags = hsc_dflags hsc_env
         ; let logger = hsc_logger hsc_env
         ; let ic = hsc_IC hsc_env
         ; let unit_env = hsc_unit_env hsc_env
         ; let simplify_expr_opts = initSimplifyExprOpts dflags ic
         ; simpl_expr <- {-# SCC simplifyExpr #-} simplifyExpr logger (ue_eps unit_env) simplify_expr_opts ds_expr

           {- Tidy it (temporary, until coreSat does cloning) -}
         ; let tidy_expr = {-# SCC tidyExpr #-} tidyExpr emptyTidyEnv simpl_expr

           {- Prepare for codegen -}
         ; cp_cfg <- {-# SCC initCorePrepConfig #-} initCorePrepConfig hsc_env
         ; prepd_expr <- {-# SCC corePrepExpr #-} corePrepExpr
            logger cp_cfg
            tidy_expr

           {- Lint if necessary -}
         ; lintInteractiveExpr (text "hscCompileExpr") hsc_env prepd_expr
         ; let iNTERACTIVELoc = ModLocation{ ml_hs_file   = Nothing,
                                      ml_hi_file   = panic "hscCompileCoreExpr':ml_hi_file",
                                      ml_obj_file  = panic "hscCompileCoreExpr':ml_obj_file",
                                      ml_dyn_obj_file = panic "hscCompileCoreExpr': ml_obj_file",
                                      ml_dyn_hi_file  = panic "hscCompileCoreExpr': ml_dyn_hi_file",
                                      ml_hie_file  = panic "hscCompileCoreExpr':ml_hie_file" }

         ; let ictxt = hsc_IC hsc_env
         ; (binding_id, stg_expr, _, _, _stg_cg_info) <-
            {-# SCC myCoreToStgExpr #-}
             myCoreToStgExpr logger
                             dflags
                             ictxt
                             True
                             (icInteractiveModule ictxt)
                             iNTERACTIVELoc
                             prepd_expr

           {- Convert to BCOs -}
         ; bcos <-
            {-# SCC byteCodeGen #-}
                byteCodeGen hsc_env
                     (icInteractiveModule ictxt)
                     stg_expr
                     [] Nothing

           {- load it -}
         ; (fv_hvs, mods_needed, units_needed) <- {-# SCC loadDecls #-} loadDecls (hscInterp hsc_env) hsc_env srcspan bcos
           {- Get the HValue for the root -}
         ; return (expectJust "hscCompileCoreExpr'"
              $ lookup (idName binding_id) fv_hvs, mods_needed, units_needed) }

simplifyExpr :: Logger
             -> ExternalUnitCache
             -> SimplifyExprOpts
             -> CoreExpr
             -> IO CoreExpr
-- simplifyExpr is called by the driver to simplify an
-- expression typed in at the interactive prompt
simplifyExpr logger euc opts expr
  = withTiming logger (text "Simplify [expr]") (const ()) $
    do  { eps <- eucEPS euc ;
        ; let fam_envs = ( eps_fam_inst_env eps
                         , extendFamInstEnvList emptyFamInstEnv $ se_fam_inst opts
                         )
              simpl_env = mkSimplEnv (se_mode opts) fam_envs
              top_env_cfg = se_top_env_cfg opts
              read_eps_rules = eps_rule_base <$> eucEPS euc
              read_ruleenv = updExternalPackageRules emptyRuleEnv <$> read_eps_rules

        ; let sz = exprSize expr

        ; (expr', counts) <- initSmpl logger read_ruleenv top_env_cfg sz $
                             simplExprGently simpl_env expr

        ; Logger.putDumpFileMaybe logger Opt_D_dump_simpl_stats
                  "Simplifier statistics" FormatText (pprSimplCount counts)

        ; Logger.putDumpFileMaybe logger Opt_D_dump_simpl "Simplified expression"
                        FormatCore
                        (pprCoreExpr expr')

        ; return expr'
        }

simplExprGently :: SimplEnv -> CoreExpr -> SimplM CoreExpr
-- Simplifies an expression
--      does occurrence analysis, then simplification
--      and repeats (twice currently) because one pass
--      alone leaves tons of crud.
-- Used (a) for user expressions typed in at the interactive prompt
--      (b) the LHS and RHS of a RULE
--      (c) Template Haskell splices
--
-- The name 'Gently' suggests that the SimplMode is InitialPhase,
-- and in fact that is so.... but the 'Gently' in simplExprGently doesn't
-- enforce that; it just simplifies the expression twice

-- It's important that simplExprGently does eta reduction; see
-- Note [Simplify rule LHS] above.  The
-- simplifier does indeed do eta reduction (it's in GHC.Core.Opt.Simplify.completeLam)
-- but only if -O is on.

simplExprGently env expr = do
    expr1 <- simplExpr env (occurAnalyseExpr expr)
    simplExpr env (occurAnalyseExpr expr1)

myCoreToStgExpr :: Logger -> DynFlags -> InteractiveContext
                -> Bool
                -> Module -> ModLocation -> CoreExpr
                -> IO ( Id
                      , [CgStgTopBinding]
                      , InfoTableProvMap
                      , CollectedCCs
                      , StgCgInfos )
myCoreToStgExpr logger dflags ictxt for_bytecode this_mod ml prepd_expr = do
    {- Create a temporary binding (just because myCoreToStg needs a
       binding for the stg2stg step) -}
    let bco_tmp_id = mkSysLocal (fsLit "BCO_toplevel")
                                (mkPseudoUniqueE 0)
                                ManyTy
                                (GHC.Core.Utils.exprType prepd_expr)
    (stg_binds, prov_map, collected_ccs, stg_cg_infos) <-
       myCoreToStg logger
                   dflags
                   ictxt
                   for_bytecode
                   this_mod
                   ml
                   [NonRec bco_tmp_id prepd_expr]
    return (bco_tmp_id, stg_binds, prov_map, collected_ccs, stg_cg_infos)

myCoreToStg :: Logger -> DynFlags -> InteractiveContext
            -> Bool
            -> Module -> ModLocation -> CoreProgram
            -> IO ( [CgStgTopBinding] -- output program
                  , InfoTableProvMap
                  , CollectedCCs -- CAF cost centre info (declared and used)
                  , StgCgInfos )
myCoreToStg logger dflags ictxt for_bytecode this_mod ml prepd_binds = do
    let (stg_binds, denv, cost_centre_info)
         = {-# SCC "Core2Stg" #-}
           coreToStg (initCoreToStgOpts dflags) this_mod ml prepd_binds

    (stg_binds_with_fvs,stg_cg_info)
        <- {-# SCC "Stg2Stg" #-}
           stg2stg logger (interactiveInScope ictxt) (initStgPipelineOpts dflags for_bytecode)
                   this_mod stg_binds

    putDumpFileMaybe logger Opt_D_dump_stg_cg "CodeGenInput STG:" FormatSTG
        (pprGenStgTopBindings (initStgPprOpts dflags) stg_binds_with_fvs)

    return (stg_binds_with_fvs, denv, cost_centre_info, stg_cg_info)

handleWarnings :: Hsc ()
handleWarnings = do
    diag_opts <- initDiagOpts <$> getDynFlags
    print_config <- initPrintConfig <$> getDynFlags
    logger <- getLogger
    w <- getDiagnostics
    liftIO $ printOrThrowDiagnostics logger print_config diag_opts w
    clearDiagnostics

getDiagnostics :: Hsc (Messages GhcMessage)
getDiagnostics = Hsc $ \_ w -> return (w, w)

clearDiagnostics :: Hsc ()
clearDiagnostics = Hsc $ \_ _ -> return ((), emptyMessages)

logDiagnostics :: Messages GhcMessage -> Hsc ()
logDiagnostics w = Hsc $ \_ w0 -> return ((), w0 `unionMessages` w)


-- | Compile a parsed expression (before renaming), run it, and deliver
-- the resulting HValue.
compileParsedExprRemote :: GhcMonad m => LHsExpr GhcPs -> m ForeignHValue
compileParsedExprRemote expr@(L loc _) = withSession $ \hsc_env -> do
  let dflags = hsc_dflags hsc_env
  let interp = hscInterp hsc_env

  -- > let _compileParsedExpr = expr
  -- Create let stmt from expr to make hscParsedStmt happy.
  -- We will ignore the returned [Id], namely [expr_id], and not really
  -- create a new binding.
  let expr_fs = fsLit "_compileParsedExpr"
      loc' = locA loc
      expr_name = mkInternalName (getUnique expr_fs) (mkTyVarOccFS expr_fs) loc'
      let_stmt = L loc . LetStmt noAnn . (HsValBinds noAnn) $
        ValBinds NoAnnSortKey
                     (unitBag $ mkHsVarBind loc' (getRdrName expr_name) expr) []

  pstmt <- liftIO $ hscParsedStmt hsc_env let_stmt
  let (hvals_io, fix_env) = case pstmt of
        Just ([_id], hvals_io', fix_env') -> (hvals_io', fix_env')
        _ -> panic "compileParsedExprRemote"

  updateFixityEnv fix_env
  let eval_opts = initEvalOpts dflags False
  status <- liftIO $ evalStmt interp eval_opts (EvalThis hvals_io)
  case status of
    EvalComplete _ (EvalSuccess [hval]) -> return hval
    EvalComplete _ (EvalException e) ->
      liftIO $ throwIO (fromSerializableException e)
    _ -> panic "compileParsedExpr"

compileParsedExpr :: GhcMonad m => LHsExpr GhcPs -> m HValue
compileParsedExpr expr = do
   fhv <- compileParsedExprRemote expr
   interp <- hscInterp <$> getSession
   liftIO $ wormhole interp fhv

rttiEnvironment :: HscEnv -> IO HscEnv
rttiEnvironment hsc_env@HscEnv{hsc_IC=ic} = do
   let tmp_ids = [id | AnId id <- ic_tythings ic]
       incompletelyTypedIds =
           [id | id <- tmp_ids
               , not $ noSkolems id
               , (occNameFS.nameOccName.idName) id /= result_fs]
   foldM improveTypes hsc_env (map idName incompletelyTypedIds)
    where
     noSkolems = noFreeVarsOfType . idType
     improveTypes hsc_env@HscEnv{hsc_IC=ic} name = do
      let tmp_ids = [id | AnId id <- ic_tythings ic]
          Just id = find (\i -> idName i == name) tmp_ids
      if noSkolems id
         then return hsc_env
         else do
           mb_new_ty <- reconstructType hsc_env 10 id
           let old_ty = idType id
           case mb_new_ty of
             Nothing -> return hsc_env
             Just new_ty -> do
              case improveRTTIType hsc_env old_ty new_ty of
               Nothing -> warnPprTrace True (":print failed to calculate the "
                                             ++ "improvement for a type")
                              (vcat [ text "id" <+> ppr id
                                    , text "old_ty" <+> debugPprType old_ty
                                    , text "new_ty" <+> debugPprType new_ty ]) $
                          return hsc_env
               Just subst -> do
                 let logger = hsc_logger hsc_env
                 putDumpFileMaybe logger Opt_D_dump_rtti "RTTI"
                   FormatText
                   (fsep [text "RTTI Improvement for", ppr id, equals,
                          ppr subst])

                 let ic' = substInteractiveContext ic subst
                 return hsc_env{hsc_IC=ic'}


result_fs :: FastString
result_fs = fsLit "_result"

-- | Update fixity environment in the current interactive context.
updateFixityEnv :: GhcMonad m => FixityEnv -> m ()
updateFixityEnv fix_env = do
  hsc_env <- getSession
  let ic = hsc_IC hsc_env
  setSession $ hsc_env { hsc_IC = ic { ic_fix_env = fix_env } }

