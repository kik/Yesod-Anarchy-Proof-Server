module Handler.Problem where

import Import hiding (sequence_)
import Util
import Checker
import Control.Monad (when, void)
import qualified Data.Text as T
import Data.Foldable
import Data.Time.Clock (getCurrentTime)

getProblemListR :: Handler RepHtml
getProblemListR = do
  problems <- runDB $ selectList [] [Asc ProblemId]
  defaultLayout $ do
    h2id <- lift newIdent
    setTitle "AnarchyProofServer homepage"
    $(widgetFile "problem-list")

data Prob = Prob
            { probTitle :: Text
            , probDescription :: Textarea
            , probDefinitions :: Textarea
            , probTheorem :: Textarea
            , probVerifier :: Textarea
            , probAssumption :: Textarea
            }

probForm :: Form Prob
probForm =
  renderDivs $ Prob
  <$> areq textField "Title" Nothing
  <*> areq textareaField "Description" Nothing
  <*> areq textareaField "Definitions" Nothing
  <*> areq textareaField "Theorem" Nothing
  <*> areq textareaField "Vefifier" Nothing
  <*> areq textareaField "Assumption" Nothing

getProblemNewR :: Handler RepHtml
getProblemNewR = do
  (probWidget, probEnctype) <- generateFormPost $ probForm
  defaultLayout $ do
    setTitle "AnarchyProofServer homepage"
    $(widgetFile "problem-new")

postProblemConfirmNewR :: Handler RepHtml
postProblemConfirmNewR = do
  ((result, probWidget), probEnctype) <- runFormPost probForm
  prob <- case result of
    FormSuccess r -> return r
    _ -> redirect $ ProblemNewR
  --(ok, compileLog) <- withTempDir "aps-" $ checkProb prob
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

getProblemViewAnswerR :: AnswerId -> Handler RepHtml
getProblemViewAnswerR answerId = do
  answer <- runDB $ get404 answerId
  let problemId = answerProblemId answer
  let widget = textWidget $ answerFile answer
  problem <- runDB $ getJust problemId
  defaultLayout $ do
    setTitle "AnarchyProofServer homepage"
    $(widgetFile "problem-view-answer")  

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
      [a | a <- as, answerLanguageId (entityVal a) == lk]

postProblemSolveR :: ProblemId -> Handler RepHtml
postProblemSolveR problemId = do
  problem <- runDB $ get404 problemId
  ((result, _), _) <- runFormPost ansForm
  answer <- case result of
        FormSuccess r -> return r
        _ -> redirect $ ProblemViewR problemId
  (error, compileLog) <- runCheck "aps-" $ checkAns problem answer
  let ok = error == Right ()
  when ok $ saveAnswer answer
  defaultLayout $ do
    setTitle "AnarchyProofServer homepage"
    $(widgetFile "post-answer")
  where
    saveAnswer ans = do
      let languageId = entityKey $ ansLang ans
      prev <- runDB $ getBy $ AnswerProblemLanguageUser problemId languageId (ansUser ans)
      current <- liftIO $ getCurrentTime
      insertOrUpdate prev 
        $ Answer
        { answerProblemId = problemId
        , answerLanguageId = languageId
        , answerUser = ansUser ans
        , answerFile = ansFile ans
        , answerSize = T.length $ ansFile ans
        , answerCreatedAt = current
        }
    insertOrUpdate Nothing new =
      void $ runDB $ insert new
    insertOrUpdate (Just (Entity id _)) new =
      runDB $ replace id new

checkAns :: Problem -> Ans -> Check Handler ()
checkAns problem answer = do
  execWordCheck $ ansFile answer
  sequence_ coqcDefinitions
  coqcInput
  coqcVerify
  used <- coqchkInput
  execAxiomCheck used axioms
  where
    coqcDefinitions = coqc "Definition" [] <$> problemDefinitions problem
    coqcInput       = coqc "Input.v" [] $ ansFile answer
    coqcVerify      = coqc "Verify.v" verifyOpt $ problemVerifier problem
    
    requireInput = "-require Input"
    requireDefinitions = 
      maybe [] (const ["-require Definitions"]) coqcDefinitions
    verifyOpt = requireInput : requireDefinitions
    
    coqchkInput = coqchk "Input" ["-o", "-norec"]
    axioms = maybe [] T.lines $ problemAssumption problem
    compiler = "coqc" -- TODO: ansLang answer
    coqc name optlist content = 
      execCoqc $ ShellSpec { sourceFileName = name
                           , sourceFileContent = Just content
                           , commandName = compiler
                           , commandOptions = optlist
                           }
    coqchk name optlist = 
      execCoqchk $ ShellSpec { sourceFileName = name
                             , sourceFileContent = Nothing
                             , commandName = "coqchk"
                             , commandOptions = optlist
                             }

{-
checkProb :: Prob -> FilePath -> Handler (Bool, Widget)
checkProb prob tmpdir =
  runCommands rcs
  where
    rcs = rcWordCheck : toList rcDefinitions ++ [rcInput, rcVerify, rcCheck]

    rcWordCheck1  = CheckWordCommand $ ansFile answer
    rcDefinitions = coqc "Definition" [] <$> problemDefinitions problem
    rcInput       = coqc "Input.v" [] $ ansFile answer
    rcVerify      = coqc "Verify.v" verifyOpt $ problemVerifier problem
    requireInput = "-require Input"
    requireDefinitions = 
      maybe [] (const ["-require Definitions"]) rcDefinitions
    verifyOpt = requireInput : requireDefinitions
    rcCheck = coqcheck "Input" ["-o", "-norec"] $ problemAssumption problem

    compiler = "coqc" -- TODO: ansLang answer
    coqc name optlist content = ShellCommand
         { workingDirectory = tmpdir
         , sourceFileName = name
         , sourceFileContent = Just content
         , commandSpec = CoqcCommand compiler
         , commandOptions = optlist
         }
    coqcheck name optlist axioms = ShellCommand
         { workingDirectory = tmpdir
         , sourceFileName = name
         , sourceFileContent = Nothing
         , commandSpec = CoqcheckCommand "coqchk" axioms
         , commandOptions = optlist
         }
-}