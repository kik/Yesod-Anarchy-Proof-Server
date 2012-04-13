module Handler.Problem where

import Import
import Util
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy.Encoding (decodeUtf8')
import Data.Text.Lazy (toStrict)
import Data.Foldable
import System.FilePath ((</>))
import System.IO
import Data.Time.Clock (getCurrentTime)
import Control.Arrow ((&&&))

getProblemListR :: Handler RepHtml
getProblemListR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "AnarchyProofServer homepage"
        [whamlet|TODO|]

textWidget :: Text -> GWidget sub master ()
textWidget t = 
  foldMap addBr $ T.lines t
    where
      addBr l = [whamlet|
                 #{l}
                 <br>|]

data Ans = Ans
           { ansUser :: Text
           , ansLang :: Entity Language
           , ansFile :: FileInfo
           }
           
ansForm :: Form Ans
ansForm =
  renderDivs $ Ans
  <$> areq textField "User" Nothing
  <*> areq langField "Lang" Nothing
  <*> fileAFormReq "File"
    where
      langOpts = optionsPersist [] [Asc LanguageId] languageName
      langField = selectField langOpts

answerForm :: ProblemId -> Form Answer
answerForm problemId extra = do
  (userRes, userView) <- mreq textField "User" Nothing
  (langRes, langView) <- mreq langField "Lang" Nothing
  (fileRes, fileView) <- textFileMFormReq "File"
  current <- currentTime
  let answerRes = Answer problemId
                  <$> (entityKey <$> langRes)
                  <*> userRes
                  <*> fileRes
                  <*> (T.length <$> fileRes)
                  <*> pure current
  let widget = [whamlet|TODO|]
  return (answerRes, widget)
  where
    langOpts = optionsPersist [] [Asc LanguageId] languageName
    langField = selectField langOpts
    currentTime = liftIO $ getCurrentTime

textFileMFormReq :: 
  RenderMessage master FormMessage => 
  FieldSettings master -> MForm sub master (FormResult Text, GWidget sub master ())
textFileMFormReq fs = do
  (fileInfoRes, fileView) <- aFormToForm $ fileAFormReq fs
  let fileRes = check $ utf8dec <$> fileInfoRes
  let widget = [whamlet|TODO|]
  return (toStrict <$> fileRes, widget)
  where
    utf8dec = decodeUtf8' . fileContent
    check (FormSuccess (Right t)) = pure t
    check (FormSuccess _) = FormFailure ["failed to decode utf-8"]
    check FormMissing = FormMissing
    check (FormFailure x) = FormFailure x

getProblemViewR :: ProblemId -> Handler RepHtml
getProblemViewR problemId = do
  problem <- runDB $ get404 problemId
  answers <- runDB $ selectList [AnswerProblemId ==. problemId] [Asc AnswerSize, Asc AnswerCreatedAt]
  langs   <- runDB $ selectList [] [Asc LanguageId]
  (ansWidget, ansEnctype) <- generateFormPost $ ansForm
  defaultLayout $ do
    setTitle "AnarchyProofServer homepage"
    let desc = textWidget $ problemDescription problem
    let defs = textWidget <$> problemDefinitions problem
    let thm  = textWidget $ problemTheorem problem
    let verf = textWidget $ problemVerifier problem
    let assm = textWidget <$> problemAssumption problem
    $(widgetFile "problem-view")
  where
    grouping ls as =
      [(l, groupByLang lk as) | Entity lk l <- ls]
    groupByLang lk as =
      [a | Entity _ a <- as, answerLanguageId a == lk]

postProblemSolveR :: ProblemId -> Handler RepHtml
postProblemSolveR problemId = do
  problem <- runDB $ get404 problemId
  ((result, _), _) <- runFormPost ansForm
  answer <- case result of
        FormSuccess r -> return r
        _ -> redirect $ ProblemViewR problemId
  withTempDir "aps-" $ checkAns problem answer  
  defaultLayout $ do
    setTitle "AnarchyProofServer homepage"
    [whamlet|TODO|]

checkAns :: Problem -> Ans -> FilePath -> Handler ()
checkAns problem answer tmpdir = do
  for_ (problemDefinitions problem) 
    $ saveT "Definitions.v"
  saveBS "Input.v" $ fileContent $ ansFile answer
  saveT "Verify.v" $ problemVerifier problem
  return ()
  where
    path name = tmpdir </> name
    savef name f = liftIO $ do
      withFile (path name) WriteMode $ \h -> do
        f h
    saveT name text = 
      savef name 
      $ \h -> hSetEncoding h utf8 >> TIO.hPutStr h text
    saveBS name bs =
      savef name $ flip B.hPut bs
  
