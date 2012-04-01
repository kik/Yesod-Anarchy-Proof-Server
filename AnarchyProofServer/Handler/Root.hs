module Handler.Root where

import Import
import Data.Text as T

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "AnarchyProofServer homepage"
        $(widgetFile "homepage")

getProblemListR :: Handler RepHtml
getProblemListR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "AnarchyProofServer homepage"
        $(widgetFile "homepage")

getProblemViewR :: ProblemId -> Handler RepHtml
getProblemViewR problem_id = do
  problem <- runDB $ get404 problem_id
  defaultLayout $ do
    setTitle "AnarchyProofServer homepage"
    let descLines = T.lines $ problemDescription problem
    let defsLines = T.lines $ problemDefinitions problem
    let hasDefs = defsLines /= []
    let thmLines  = T.lines $ problemTheorem problem
    let verfLines = T.lines $ problemVerifier problem
    let assmLines = T.lines $ problemAssumption problem
    let hasAssm = assmLines /= []
    $(widgetFile "problem-view")
  
