{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

-- | External interpreter program
module GHC.Runtime.Interpreter.C
  ( generateIservC
  )
where

import GHC.Prelude
import GHC.Platform
import GHC.Platform.Ways
import GHC.Data.FastString
import GHC.Driver.Session
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import GHC.Unit.Types
import GHC.Unit.Env
import GHC.Unit.Info
import GHC.Unit.State
import GHC.Utils.Panic.Plain
import GHC.Linker.Executable
import GHC.Linker.Config

import GHC.Fingerprint (fingerprintString)
import System.Directory (doesFileExist, createDirectoryIfMissing, getPermissions, executable, copyFile, setPermissions)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)

-- | Generate iserv program for the target.
--
-- The linked binary is cached based on the GHC libdir and ways so that
-- subsequent GHC invocations with the same configuration can reuse it
-- without re-linking (~1.3s savings on macOS).
generateIservC :: DynFlags -> Logger -> TmpFs -> ExecutableLinkOpts -> UnitEnv -> IO FilePath
generateIservC dflags logger tmpfs opts unit_env = do
  -- get the unit-id of the ghci package. We need this to load the
  -- interpreter code.
  let unit_state = ue_homeUnitState unit_env
  ghci_unit_id <- case lookupPackageName unit_state (PackageName (fsLit "ghci")) of
    Nothing -> cmdLineErrorIO "C interpreter: couldn't find \"ghci\" package"
    Just i  -> pure i

  -- Compute the final ways for iserv (same logic as used below in opts')
  let final_ways =
        let ways = leWays opts
            ways' = addWay WayThreaded ways
        in if targetHasRTSWays dflags ways' then ways' else ways

  -- Compute a deterministic cache key from the GHC installation and ways.
  -- topDir uniquely identifies the GHC installation (includes nix store hash,
  -- version-specific paths, etc.). Combined with ways and ghci unit-id,
  -- this fully determines the iserv binary.
  let cache_key = show $ fingerprintString $ concat
        [ topDir dflags
        , unitIdString ghci_unit_id
        , show final_ways
        ]
  let cache_dir = "/tmp/ghc-iserv-cache"
  let cache_file = cache_dir </> cache_key

  -- Check if we have a cached iserv binary
  cached <- iservCacheValid cache_file
  if cached
    then pure cache_file
    else do
      -- Link a fresh iserv binary
      exe_file <- linkIserv dflags logger tmpfs opts unit_env ghci_unit_id final_ways

      -- Store in cache for future invocations (best-effort, don't fail if
      -- caching doesn't work, e.g. due to permissions)
      _ <- try @SomeException $ do
        createDirectoryIfMissing True cache_dir
        copyFileToCache exe_file cache_file

      -- Return the cached file if it was stored successfully, otherwise
      -- fall back to the temp file
      cached' <- iservCacheValid cache_file
      pure (if cached' then cache_file else exe_file)

-- | Link a fresh iserv binary and return its path
linkIserv :: DynFlags -> Logger -> TmpFs -> ExecutableLinkOpts -> UnitEnv
          -> UnitId -> Ways -> IO FilePath
linkIserv _dflags logger tmpfs opts unit_env ghci_unit_id final_ways = do
  let tmpdir = leTempDir opts
  exe_file <- newTempName logger tmpfs tmpdir TFL_GhcSession "iserv"

  let platform = ue_platform unit_env
  let os       = platformOS platform

  -- we inherit ExecutableLinkOpts for the target code (i.e. derived from
  -- DynFlags specified by the user and from settings). We need to adjust these
  -- options to generate the iserv program we want. Some settings are to be
  -- shared (e.g. ways, platform, etc.) but some other must be set specifically
  -- for iserv.
  let opts' = opts
        { -- write iserv program in some temporary directory
          leOutputFile = Just exe_file

          -- we need GHC to generate a main entry point...
        , leNoHsMain = False

          -- ...however the main symbol must be the iserv server
        , leMainSymbol = zString (zEncodeFS (unitIdFS ghci_unit_id)) ++ "_GHCiziServer_defaultServer"

          -- we need to reset inputs, otherwise one of them may be defining
          -- `main` too (with -no-hs-main).
        , leInputs = []

          -- we never know what symbols GHC will look up in the future, so we
          -- must retain CAFs for running interpreted code.
        , leKeepCafs = True

          -- link with -threaded if target has threaded RTS
        , leWays = final_ways

          -- enable all rts options
        , leRtsOptsEnabled = RtsOptsAll

          -- Add -Wl,--export-dynamic enables GHCi to load dynamic objects that
          -- refer to the RTS.  This is harmless if you don't use it (adds a bit
          -- of overhead to startup and increases the binary sizes) but if you
          -- need it there's no alternative.
          --
          -- The Solaris linker does not support --export-dynamic option. It also
          -- does not need it since it exports all dynamic symbols by default
        , leLinkerConfig = if
            | osElfTarget os
            , os /= OSFreeBSD
            , os /= OSSolaris2
            -> (leLinkerConfig opts)
                { linkerOptionsPost = linkerOptionsPost (leLinkerConfig opts) ++ [Option "-Wl,--export-dynamic"]
                }
            | otherwise
            -> leLinkerConfig opts
        }
  linkExecutable logger tmpfs opts' unit_env [] [ghci_unit_id]
  pure exe_file

-- | Check if a cached iserv binary exists and is executable
iservCacheValid :: FilePath -> IO Bool
iservCacheValid path = do
  exists <- doesFileExist path
  if exists
    then do
      perms <- getPermissions path
      pure (executable perms)
    else pure False

-- | Copy the linked iserv binary to the cache location, preserving
-- executable permissions.
copyFileToCache :: FilePath -> FilePath -> IO ()
copyFileToCache src dst = do
  copyFile src dst
  -- Ensure the cached binary is executable
  perms <- getPermissions dst
  setPermissions dst (perms { executable = True })
