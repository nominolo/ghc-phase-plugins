module Ghc.Api.Main where

import Ghc.Api.V76Hsc ( FileHooks(..), defaultHooks, Hook, HHsc )
import Ghc.Api.V76

import GHC as Ghc
import MonadUtils ( liftIO )
import HscTypes ( CgGuts(..) )
import GHC.Paths ( libdir )
import Bag ( emptyBag )

ghcMain :: FilePath -> IO ()
ghcMain file = do
  runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    let dflags1a = dflags0{ ghcLink = NoLink
                          , hscTarget = HscAsm
                          , ghcMode = OneShot
                          }
        -- dflags1 = updOptLevel (Cli.optLevel opts) dflags1a
        -- dflags2 | Cli.package_name opts /= ""
        --         = setPackageName (Cli.package_name opts) dflags1
        --         | otherwise = dflags1
        -- dflags3 = dflags2{ systemPackageConfig = dbPath }
        -- dflags = dopt_unset dflags3 Opt_ReadUserPackageConf
        dflags = dflags1a
    setSessionDynFlags dflags

    hsc_env <- getSession

    let hooks = defaultHooks
                { hookCodeGen = myGenBytecode
                , hookPostBackendPhase = \_default _dflags _src _lang ->
                                            StopLn
                }

    liftIO $ compileOneShot hsc_env hooks StopLn [(file, Nothing)]

    return ()


myGenBytecode :: Hook (ModIface -> ModDetails -> CgGuts -> ModSummary
                                -> HHsc (Maybe FilePath))
myGenBytecode defaultImpl modIface modDetails cgGuts modSummary = do
  print "bytecode invoked"
  return ((emptyBag, emptyBag), Just Nothing)