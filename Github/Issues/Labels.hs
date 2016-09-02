-- | The API for dealing with labels on Github issues, as described on
-- <http://developer.github.com/v3/issues/labels/>.
module Github.Issues.Labels (
 label
,labelsOnRepo
,labelsOnRepo'
,labelsOnIssue
,labelsOnMilestone
,createLabel
,applyLabels
,listLabels
,removeLabels
,removeAllLabels
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All the labels available to use on any issue in the repo.
--
-- > labelsOnRepo "thoughtbot" "paperclip"
labelsOnRepo :: String -> String -> IO (Either Error [IssueLabel])
labelsOnRepo user reqRepoName = githubGet ["repos", user, reqRepoName, "labels"]

labelsOnRepo' :: GithubAuth -> String -> String -> IO (Either Error [IssueLabel])
labelsOnRepo' auth user reqRepoName =
  githubGet' (Just auth) ["repos", user, reqRepoName, "labels"]

-- | The labels on an issue in a repo.
--
-- > labelsOnIssue "thoughtbot" "paperclip" 585
labelsOnIssue :: String -> String -> Int ->  IO (Either Error [IssueLabel])
labelsOnIssue user reqRepoName reqIssueId =
  githubGet ["repos", user, reqRepoName, "issues", show reqIssueId, "labels"]

-- | All the labels on a repo's milestone, given the milestone ID.
--
-- > labelsOnMilestone "thoughtbot" "paperclip" 2
labelsOnMilestone :: String -> String -> Int ->  IO (Either Error [IssueLabel])
labelsOnMilestone user reqRepoName milestoneId =
  githubGet ["repos", user, reqRepoName, "milestones", show milestoneId, "labels"]

-- | A label, by name.
--
-- > Github.label "thoughtbot" "paperclip" "bug"
label :: String -> String -> String -> IO (Either Error IssueLabel)
label user reqRepoName reqLabelName =
  githubGet ["repos", user, reqRepoName, "labels", reqLabelName]

-- https://developer.github.com/v3/issues/labels/#create-a-label
createLabel :: GithubAuth -> String -> String -> NewLabel -> IO (Either Error IssueLabel)
createLabel auth user reqRepoName label =
  githubPost auth ["repos", user, reqRepoName, "labels"] label

-- https://developer.github.com/v3/issues/labels/#add-labels-to-an-issue
applyLabels :: GithubAuth -> String -> String -> String -> [String] -> IO (Either Error [IssueLabel])
applyLabels auth user reqRepoName issueNumber l =
  githubPost auth ["repos", user, reqRepoName, "issues", issueNumber, "labels"] l

listLabels :: GithubAuth -> String -> String -> String -> IO (Either Error [IssueLabel])
listLabels auth user reqRepoName issueNumber =
  githubGet' (Just auth) ["repos", user, reqRepoName, "issues", issueNumber, "labels"]

-- https://developer.github.com/v3/issues/labels/#remove-a-label-from-an-issue
--removeLabels :: GithubAuth -> String -> String -> String -> String -> IO (Either SomeException (Response LBS.ByteString))
removeLabels auth user reqRepoName issueNumber l =
  githubDelete auth ["repos", user, reqRepoName, "issues", issueNumber, "labels", l]

--removeAllLabels :: GithubAuth -> String -> String -> String -> IO (Either SomeException (Response LBS.ByteString))
removeAllLabels auth user reqRepoName issueNumber =
  githubDelete auth ["repos", user, reqRepoName, "issues", issueNumber, "labels"]
