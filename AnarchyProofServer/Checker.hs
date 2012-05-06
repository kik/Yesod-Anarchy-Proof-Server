module Checker where

import Import hiding (elem, all, catch)
import Util
import Data.List ((\\))
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import System.FilePath ((</>))
import System.Exit
import System.Process (system)
import Data.Foldable
import Control.Monad.Trans.Error
import Control.Monad (unless, void)
import Control.Exception.Lifted (catch)
import Control.Monad.RWS (RWST(..), tell, ask)
import Control.Monad (when)

data ShellSpec = ShellSpec
               { sourceFileName :: FilePath
               , sourceFileContent :: Maybe Text
               , commandName :: String
               , commandOptions :: [String]
               }
               deriving Show

type Check m a = ErrorT IOError (RWST FilePath Widget () m) a

runCheck :: FilePath -> Check Handler a -> Handler (Either IOError a, Widget)
runCheck tdirBase f = 
  withTempDir tdirBase $ \tdir -> do
    (x, _, y) <- runRWST go tdir ()
    return (x, y)
  where
    go = runErrorT f

execCoqc :: ShellSpec -> Check Handler ()
execCoqc spec = do
  void $ execShell spec
  
execCoqchk :: ShellSpec -> Check Handler [Text]
execCoqchk spec = do
  axioms <$> execShell spec
  where
    parse ls = map T.strip 
               $ drop 1 
               $ dropWhile (not . T.isPrefixOf "* Axioms:") ls
    axioms ls = parse ls \\ [""]

execShell :: ShellSpec 
             -> Check Handler [Text]
execShell spec@(ShellSpec { sourceFileName = src
                          , sourceFileContent = srcContent
                          , commandName = cmd
                          , commandOptions = optlist}) = do
  lift $ lift $ $(logDebug) $ T.pack $ cmd ++ show spec
  wdir <- ask
  run wdir `catch` throwError
    
  where    
    run wdir = do 
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
      where
        commandLine = unwords $ ["cd", wdir, ";", cmd] ++ optlist ++ src : redir
        path name = wdir </> name
        outfile = path "t.out"
        errfile = path "t.err"
        redir = [">", outfile, "2>", errfile]

execWordCheck :: Text -> Check Handler ()
execWordCheck t =
  case found of
    Just w -> throwError $ userError $ "You can not use '" ++ T.unpack w ++ "'."
    _ -> return ()
  where
    found = find (`T.isInfixOf` t) forbidden
    forbidden =
      T.words "Pwd Cd Drop ProtectedLoop Load Declare LoadPath Path ML State Debug Extract"

execAxiomCheck :: [Text] -> [Text] -> Check Handler ()
execAxiomCheck used declared =
  when (diff /= []) err
  where
    diff = used \\ declared
    err = throwError $ userError $ "You can not assume:\n" ++ T.unpack (T.unlines diff)
