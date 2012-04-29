module Handler.Problem where

import Import
import Util
import qualified Data.Text as T
import Data.Foldable
import System.FilePath ((</>))
import Data.Time.Clock (getCurrentTime)
import System.Process (system)
import System.Exit
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import Control.Monad.Trans.Writer

getProblemListR :: Handler RepHtml
getProblemListR = do
  problems <- runDB $ selectList [] [Asc ProblemId]
  defaultLayout $ do
    h2id <- lift newIdent
    setTitle "AnarchyProofServer homepage"
    $(widgetFile "problem-list")

getProblemNewR :: Handler RepHtml
getProblemNewR = do
    defaultLayout $ do
        setTitle "AnarchyProofServer homepage"
        [whamlet|TODO|]

getProblemRecentAnswersR :: Handler RepHtml
getProblemRecentAnswersR = do
  as <- runDB $ selectList [] [Desc AnswerCreatedAt, LimitTo 100]
  answers <- mapM getChild as
  defaultLayout $ do
        setTitle "AnarchyProofServer homepage"
        $(widgetFile "problem-recent-answers")
  where
    getChild (Entity _ answer) = do
      problem <- runDB $ getJust $ answerProblemId answer
      language <- runDB $ getJust $ answerLanguageId answer
      return (answer, problem, language)

data Ans = Ans
           { ansUser :: Text
           , ansLang :: Entity Language
           , ansFile :: Text
           }           

ansForm :: Form Ans
ansForm =
  renderDivs $ Ans
  <$> areq textField "User" Nothing
  <*> areq langField "Lang" Nothing
  <*> textFileAFormReq "File"
    where
      langOpts = optionsPersist [] [Asc LanguageId] languageName
      langField = selectField langOpts

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
  (ok, compileLog) <- withTempDir "aps-" $ checkAns problem answer  
  defaultLayout $ do
    setTitle "AnarchyProofServer homepage"
    $(widgetFile "post-answer")

checkAns :: Problem -> Ans -> FilePath -> Handler (Bool, Widget)
checkAns problem answer tmpdir =
  runWriterT $ go rcs
  where
    go [] = return True
    go (x:xs) = do
      (ok, widget) <- lift $ execCommand x
      tell widget
      if ok
        then go xs
        else return False
        
    rcs = toList rcDefinitions ++ [rcInput, rcVerify]
    
    rcDefinitions = coqc "Definition" [] <$> problemDefinitions problem
    rcInput =       coqc "Input.v" [] $ ansFile answer
    rcVerify =      coqc "Verify.v" verifyOpt $ problemVerifier problem
    requireInput = "-require Input"
    requireDefinitions = 
      maybe [] (const ["-require Definitions"]) rcDefinitions
    verifyOpt = requireInput : requireDefinitions

    compiler = "coqc" -- TODO: ansLang answer
    coqc name optlist content = Command
         { workingDirectory = tmpdir
         , sourceFileName = name
         , sourceFileContent = Just content
         , commandSpec = CoqcCommand compiler
         , commandOptions = optlist
         }
