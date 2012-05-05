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
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error
import Control.Monad

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

linesWidget :: [Text] -> GWidget sub master ()
linesWidget = foldMap lineWidget

textWidget :: Text -> GWidget sub master ()
textWidget = linesWidget . T.lines

textWidgetSink :: MonadResource m => Sink Text m (GWidget sub master ())
textWidgetSink =
  CT.lines =$ CL.fold app mempty
  where
    app x y = x `mappend` (lineWidget y)

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

