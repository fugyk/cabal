module Distribution.Client.GC
    ( gcAction ) where

import Distribution.Simple.Utils ( notice )
import Distribution.Verbosity ( Verbosity )
import Distribution.Simple.Setup ( Flag(..) )
import Distribution.Client.Setup ( GlobalFlags
                                 , globalConfigFile)
import Distribution.Client.Config ( loadConfig
                                  , savedConfigureFlags)
import Distribution.Simple.Configure ( configCompilerAuxEx
                                     , getInstalledPackages)
import Distribution.Simple.Compiler ( PackageDB(..) )
import Distribution.Simple.PackageIndex ( allPackages )

import Distribution.Package (packageId)

-- | Garbage collect unreachable package in dependency graph
-- with exposed packages and package in any view as roots.
gcAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
gcAction (Flag verbosity) _ globalFlags = do
    savedConfig <- loadConfig verbosity (globalConfigFile globalFlags)
    (comp, _, conf) <- configCompilerAuxEx (savedConfigureFlags savedConfig)
    allPkgsIndex <- getInstalledPackages verbosity comp pkgdbs conf
    let allPkgs = allPackages allPkgsIndex
    print $ map (display. packageId) allPkgs
    rootPkgs <- concatM $ (getPackagesInView <<= listViews)
    -- reachablePkgs <- makeGraph rootPkgs
    -- unreachablePkgs <- all_pkgs - reachablePkgs
    -- notice vebosity "These packages will be removed"
    -- notice verbosity $ intersperse '\n' (map ipid unreachablePkgs)
    -- mapM_ (deleteFolder . libraryLocation) unreachablePkgs
    -- unregister unreachablePkgs
  where
    -- Its a bit of hack to specify exact DB in source, but we want it to GC
    -- only in User package DB
    pkgdbs = [UserPackageDB]
