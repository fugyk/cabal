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
import Distribution.Simple.PackageIndex ( dependencyClosure, allPackages )

import Distribution.Package (packageId)
import Distribution.Simple.Register ( listViews, getPackagesInView )
import Distribution.InstalledPackageInfo ( installedPackageId )
import Distribution.Text ( display )

import Data.List ( deleteFirstsBy, intercalate )
import Data.Function ( on )

-- | Garbage collect unreachable package in dependency graph
-- with exposed packages and package in any view as roots.
gcAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
gcAction (Flag verbosity) _ globalFlags = do
    savedConfig <- loadConfig verbosity (globalConfigFile globalFlags)
    (comp, _, conf) <- configCompilerAuxEx (savedConfigureFlags savedConfig)
    allPkgsIndex <- getInstalledPackages verbosity comp pkgdbs conf
    let allPkgs = allPackages allPkgsIndex
    viewList <- listViews verbosity comp conf
    rootPkgsPerView <- mapM (\v -> getPackagesInView verbosity comp conf v)
                         viewList
    let rootPkgs = concat rootPkgsPerView
        depClosure = dependencyClosure allPkgsIndex rootPkgs
        reachablePkgs = case depClosure of
                          Left reachableIndex -> allPackages reachableIndex
        unreachablePkgs = deleteFirstsBy ((==) `on` installedPackageId)
                            allPkgs reachablePkgs
    notice verbosity "These packages will be removed"
    mapM_ (notice verbosity) (map (display . installedPackageId) unreachablePkgs)
    -- mapM_ (deleteFolder . libraryLocation) unreachablePkgs
    -- mapM_ unregister unreachablePkgs
  where
    -- Its a bit of hack to specify exact DB in source, but we want to GC
    -- only in User package DB
    pkgdbs = [UserPackageDB]
