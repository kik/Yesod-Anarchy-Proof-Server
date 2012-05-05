module Checker where

import Import hiding (elem, all, catch)
import Util
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
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error
import Control.Monad (unless, void)
import Control.Exception.Lifted (catch)
                    
data ShellSpec = ShellSpec
               { workingDirectory :: FilePath
               , sourceFileName :: FilePath
               , sourceFileContent :: Maybe Text
               , commandOptions :: [String]
               }
               deriving Show

type Check m a = ErrorT IOError (WriterT Widget m) a

runCheck :: Check m a -> m (Either IOError a, Widget)
runCheck = runWriterT . runErrorT

execCoqc :: ShellSpec -> String -> Check Handler ()
execCoqc spec cmd = do
  void $ execShell spec cmd
  
execCoqchk :: ShellSpec -> String -> Check Handler [Text]
execCoqchk spec cmd = do
  axioms <$> execShell spec cmd
  where
    parse lines = map T.strip 
                  $ drop 1 
                  $ dropWhile (not . T.isPrefixOf "* Axioms:") lines
    axioms lines = lines \\ [""]

execShell :: ShellSpec 
             -> String 
             -> Check Handler [Text]
execShell 
  spec@(ShellSpec { workingDirectory = wdir
                  , sourceFileName = src
                  , sourceFileContent = srcContent
                  , commandOptions = optlist}) 
  cmd = do
    lift $ lift $ $(logDebug) $ T.pack $ cmd ++ show spec
    run `catch` throwError
    
  where    
    run = do 
      forM_ srcContent 
        $ \s -> yield s $$ sinkFileUtf8 (path src)
      ex <- liftIO $ system commandLine
      outs <- sourceFileUtf8 outfile $= CT.lines $$ CL.consume
      let out = linesWidget outs
      err <- sourceFileUtf8 errfile $$ textWidgetSink
      lift $ tell $(widgetFile "compile-result")
      unless (ex == ExitSuccess)
        $ throwError $ userError $ show ex
      return outs
    commandLine = unwords $ ["cd", wdir, ";", cmd] ++ optlist ++ src : redir
    path name = wdir </> name
    outfile = path "t.out"
    errfile = path "t.err"
    redir = [">", outfile, "2>", errfile]

wordCheck :: Text -> Either Text ()
wordCheck t =
  maybe (Right ()) Left $ find (`T.isInfixOf` t) forbidden
  where
    forbidden =
      T.words "Pwd Cd Drop ProtectedLoop Load Declare LoadPath Path ML State Debug Extract"
