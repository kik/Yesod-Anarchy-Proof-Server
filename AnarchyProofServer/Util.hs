module Util where

import Import
import Control.Monad.Trans.Control (control)
import System.IO.Temp (withSystemTempDirectory)
import Data.Text.Lazy.Encoding (decodeUtf8')
import Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as TIO
import System.IO

withTempDir :: FilePath 
               -> (FilePath -> GHandler sub master a) 
               -> GHandler sub master a
withTempDir prefix a =
  control $ \runInIO ->
  withSystemTempDirectory prefix $ \tmpdir ->
  runInIO $ a tmpdir

writeFileUtf8 :: FilePath -> Text -> IO ()
writeFileUtf8 path text = do
  withFile path WriteMode $ \h -> do
    hSetEncoding h utf8
    TIO.hPutStr h text

textFileAFormReq ::
  RenderMessage master FormMessage => 
  FieldSettings master -> AForm sub master Text
textFileAFormReq fs = formToAForm $ do
  (fileInfoRes, fileView) <- aFormToForm $ fileAFormReq fs
  let fileRes = checkFail $ utf8dec <$> fileInfoRes
  return (toStrict <$> fileRes, fileView [])
  where
    utf8dec = decodeUtf8' . fileContent
    checkFail t =
      case t of
        FormSuccess (Right x) -> FormSuccess x
        FormSuccess _ -> FormFailure ["failed to decode utf-8"]
        FormMissing   -> FormMissing
        FormFailure x -> FormFailure x

