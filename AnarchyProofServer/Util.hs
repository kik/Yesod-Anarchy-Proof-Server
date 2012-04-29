module Util where

import Import hiding (elem, all)
import Data.List ((\\))
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

sinkFileUtf8 :: MonadResource m => FilePath -> Sink Text m ()  
sinkFileUtf8 path =
  CT.encode CT.utf8 =$ CB.sinkFile path
    
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
  CT.lines =$ CL.fold app mempty
  where
    app x y = x `mappend` (lineWidget y)
    
data CommandSpec = CoqcCommand
                   { coqcName :: String
                   }
                 | CoqcheckCommand
                   { coqcheckName :: String
                   , coqcheckAxioms :: Maybe Text
                   }
                 deriving Show

data Command = ShellCommand
               { workingDirectory :: FilePath
               , sourceFileName :: FilePath
               , sourceFileContent :: Maybe Text
               , commandSpec :: CommandSpec
               , commandOptions :: [String]
               }
             | CheckWordCommand Text
             deriving Show

execCommand :: Command -> Handler (Bool, Widget)
execCommand rc@(ShellCommand { workingDirectory = wdir
                             , sourceFileName = src
                             , sourceFileContent = srcContent
                             , commandSpec = commandSpec
                             , commandOptions = optlist}) = do
  $(logDebug) $ T.pack $ show rc
  forM_ srcContent 
    $ \s -> yield s $$ sinkFileUtf8 (path src)
  ex <- liftIO $ system commandLine
  out <- sourceFileUtf8 outfile $$ textWidgetSink
  err <- sourceFileUtf8 errfile $$ textWidgetSink
  let ok = ex == ExitSuccess
  postCheckErr <- if ok
                  then postCheck commandSpec
                  else return True
  return (ok && postCheckErr, $(widgetFile "compile-result"))
  where
    commandLine = unwords $ ["cd", wdir, ";", commandString commandSpec] ++ optlist ++ src : redir
    path name = wdir </> name
    outfile = path "t.out"
    errfile = path "t.err"
    redir = [">", outfile, "2>", errfile]
    commandString (CoqcCommand s) = s
    commandString (CoqcheckCommand s _) = s
    postCheck (CoqcCommand _) = return True
    postCheck (CoqcheckCommand _ axs) = do
      let allowed = maybe [] (axioms . T.lines) axs
      used <- axioms 
              <$> (sourceFileUtf8 outfile $= CT.lines $$ CL.consume)
      return $ compare allowed used
    axioms lines = map T.strip 
                   $ drop 1 
                   $ dropWhile (not . T.isPrefixOf "* Axioms:") lines
    compare allowed used = (used \\ [""]) \\ allowed == []

execCommand (CheckWordCommand src) =
  case wordCheck src of
    Right _ -> return (True, mempty)
    Left word -> return (False, $(widgetFile "word-check"))

wordCheck :: Text -> Either Text ()
wordCheck t =
  maybe (Right ()) Left $ find (`T.isInfixOf` t) forbidden
  where
    forbidden =
      T.words "Pwd Cd Drop ProtectedLoop Load Declare LoadPath Path ML State Debug Extract"

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

