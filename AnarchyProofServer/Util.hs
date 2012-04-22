module Util where

import Import
import Control.Monad.Trans.Control (control)
import System.IO.Temp (withSystemTempDirectory)
import Data.Text.Lazy.Encoding (decodeUtf8')
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import System.FilePath ((</>))
import System.Exit
import System.Process (system)
import Data.Foldable

withTempDir :: FilePath 
               -> (FilePath -> GHandler sub master a) 
               -> GHandler sub master a
withTempDir prefix a =
  control $ \runInIO ->
  withSystemTempDirectory prefix $ \tmpdir ->
  runInIO $ a tmpdir

sourceFileUtf8 :: MonadResource m => FilePath -> Source m Text
sourceFileUtf8 path =
  CB.sourceFile path $= CT.decode CT.utf8

writeFileUtf8 :: FilePath -> Text -> IO ()
writeFileUtf8 path text = do
  withFile path WriteMode $ \h -> do
    hSetEncoding h utf8
    TIO.hPutStr h text
    
    
lineWidget :: Text -> GWidget sub master ()
lineWidget t = 
  [whamlet|
   #{t}
   <br>|]

textWidget :: Text -> GWidget sub master ()
textWidget t = 
  foldMap lineWidget $ T.lines t

textWidgetSink :: MonadResource m => Sink Text m (GWidget sub master ())
textWidgetSink =
  CT.lines =$ CL.fold (flip $ mappend . lineWidget) mempty
    
data RunCompiler = RunCompiler
                   { compilerName :: String
                   , sourceFileName :: FilePath
                   , sourceFileContent :: Text
                   , compileOptions :: [String]
                   , compileDirectory :: FilePath
                   }
                   deriving Show
                   
runCompiler :: RunCompiler -> Handler (Bool, Widget)
runCompiler rc = do
  $(logDebug) $ T.pack $ show rc
  liftIO $ writeFileUtf8 (path src) srcContent
  ex <- liftIO $ system command
  out <- sourceFileUtf8 outfile $$ textWidgetSink
  err <- sourceFileUtf8 errfile $$ textWidgetSink
  return (ex == ExitSuccess, $(widgetFile "compile-result"))
  where
    command = unwords $ ["cd", tmpdir, ";", compiler, src] ++ optlist ++ redir
    tmpdir = compileDirectory rc
    path name = tmpdir </> name
    compiler = compilerName rc
    src = sourceFileName rc
    srcContent = sourceFileContent rc
    optlist = compileOptions rc
    outfile = path "t.out"
    errfile = path "t.err"
    redir = [">", outfile, "2>", errfile]

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

