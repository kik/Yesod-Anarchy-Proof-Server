module Handler.Root where

import Import
import Data.Text as T
import Data.Foldable

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

getProblemViewR :: ProblemId -> Handler RepHtml
getProblemViewR problemId = do
  problem <- runDB $ get404 problemId
  answers <- runDB $ selectList [AnswerProblemId ==. problemId] [Asc AnswerSize, Asc AnswerCreatedAt]
  langs   <- runDB $ selectList [] [Asc LanguageId]
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
