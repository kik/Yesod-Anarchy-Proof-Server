module Util where

import Import
import Control.Monad.Trans.Control (control)
import System.IO.Temp (withSystemTempDirectory)

withTempDir :: FilePath 
               -> (FilePath -> GHandler sub master a) 
               -> GHandler sub master a
withTempDir prefix a =
  control $ \runInIO ->
  withSystemTempDirectory prefix $ \tmpdir ->
  runInIO $ a tmpdir



