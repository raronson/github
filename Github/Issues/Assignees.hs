-- | The Github issues assignees events API, which is described on
-- <https://developer.github.com/v3/issues/assignees/>
module Github.Issues.Assignees (
 listAssignees
,addAssignee
,removeAssignee
,module Github.Data
) where

import Github.Data
import Github.Private

listAssignees :: GithubAuth -> String -> String -> IO (Either Error [Assignee])
listAssignees auth user reqRepoName =
  githubGet' (Just auth) ["repos", user, reqRepoName, "assignees"]

addAssignee :: GithubAuth -> String -> String -> String -> Assignees -> IO (Either Error ())
addAssignee auth user reqRepoName issueNumber assignees =
  githubPost auth ["repos", user, reqRepoName, "issues", issueNumber, "assignees"] assignees

removeAssignee :: GithubAuth -> String -> String -> String -> Assignees -> IO (Either Error ())
removeAssignee auth user reqRepoName issueNumber assignees =
  githubDeleteBody auth ["repos", user, reqRepoName, "issues", issueNumber, "assignees"] assignees
