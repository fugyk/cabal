-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Install
-- Copyright   :  (c) 2005 David Himmelstrup
--                    2007 Bjorn Bringert
--                    2007-2010 Duncan Coutts
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Garbage collection action to free unreachable packages.
-----------------------------------------------------------------------------

module Distribution.Client.GC
    ( gcAction ) where

import Distribution.Simple.Utils ( notice )
import Distribution.Verbosity ( Verbosity )
import Distribution.Simple.Setup ( Flag(..)
                                 , fromFlag
                                 , defaultRegisterFlags )
import Distribution.Client.Setup ( GlobalFlags
                                 , globalConfigFile)
import Distribution.Client.Config ( loadConfig
                                  , savedConfigureFlags )
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Compiler ( PackageDB(..) )
import Distribution.Simple.PackageIndex ( dependencyClosure
                                        , allPackages )
import Distribution.Client.Sandbox ( configCompilerAux'
                                   , configPackageDB')
import Distribution.InstalledPackageInfo ( installedPackageId
                                         , sourcePackageId
                                         , exposed )
import Distribution.Simple.Register ( listPkgenvs
                                    , getPackagesInPkgenv
                                    , unregister )
import Distribution.Text ( display )

import Data.List ( deleteFirstsBy)
import Data.Function ( on )
import Control.Monad ( when )

-- | Garbage collect unreachable package in dependency graph
-- with exposed packages and package in any package environment as roots.
gcAction :: Flag Verbosity -> [String] -> GlobalFlags -> IO ()
gcAction flagVerbosity _ globalFlags = do
    savedConfig <- loadConfig verbosity (globalConfigFile globalFlags)
    (comp, _, conf) <- configCompilerAux' (savedConfigureFlags savedConfig)
    allPkgsIndex <- getInstalledPackages verbosity comp
                      (configPackageDB' $ savedConfigureFlags savedConfig) conf
    globalPkgsIndex <- getInstalledPackages verbosity comp
                         [GlobalPackageDB] conf
    let allPkgs = allPackages allPkgsIndex
    pkgenvList <- listPkgenvs verbosity comp conf
    rootPkgsPerPkgenv <- mapM (\v -> getPackagesInPkgenv verbosity comp conf v)
                         pkgenvList
    let rootPkgs = concat rootPkgsPerPkgenv
                   -- Add all exposed packages to root packages as packages
                   -- installed by older cabal version does not adds packages
                   -- in any package environment
                   ++ map installedPackageId (filter exposed allPkgs)
                   -- Add global database packages as we don't want GC to mess
                   -- with global DB
                   ++ map installedPackageId (allPackages globalPkgsIndex)
        depClosure = dependencyClosure allPkgsIndex rootPkgs
        reachablePkgs = case depClosure of
                          Left reachableIndex -> allPackages reachableIndex
                          Right _ -> error brokenPkgsMsgs
        unreachablePkgs = deleteFirstsBy ((==) `on` installedPackageId)
                            allPkgs reachablePkgs
    notice verbosity "These packages will be removed"
    mapM_ (notice verbosity)
      (map (display . installedPackageId) unreachablePkgs)
    putStrLn "Do you want to continue (y/n):"
    opt <- getLine
    when (opt == "y") $
      -- do mapM_ (deleteFolder . libraryLocation) unreachablePkgs
      mapM_ (\pkg -> unregister (sourcePackageId pkg) [UserPackageDB] comp conf defaultRegisterFlags)
        unreachablePkgs
  where
    brokenPkgsMsgs = "There are some broken packages. GC cannot coninue. " ++
                     "Run 'ghc-pkg check' for more info"
    verbosity = fromFlag flagVerbosity
