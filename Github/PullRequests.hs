-- | The pull requests API as documented at
-- <http://developer.github.com/v3/pulls/>.
module Github.PullRequests (
 pullRequestsFor'
,pullRequestsWith'
,pullRequest'
,pullRequestCommits'
,pullRequestFiles'
,pullRequestsFor
,pullRequest
,pullRequestCommits
,pullRequestFiles
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All pull requests for the repo, by owner and repo name.
-- | With authentification
--
-- > pullRequestsFor' (Just ("github-username", "github-password")) "rails" "rails"
pullRequestsFor' :: Maybe GithubAuth -> String -> String -> IO (Either Error [PullRequest])
pullRequestsFor' auth userName reqRepoName =
  githubGet' auth ["repos", userName, reqRepoName, "pulls"]

-- | All pull requests for the repo, by owner and repo name.
-- | With authentification
--
-- > pullRequestsWith' (Just ("github-username", "github-password")) "rails" "rails"
pullRequestsWith' :: Maybe GithubAuth -> String -> String -> String -> IO (Either Error [PullRequest])
pullRequestsWith' auth userName reqRepoName state =
  githubGetWithQueryString' auth ["repos", userName, reqRepoName, "pulls"] ("state=" ++ state)

-- | All pull requests for the repo, by owner and repo name.
--
-- > pullRequestsFor "rails" "rails"
pullRequestsFor :: String -> String -> IO (Either Error [PullRequest])
pullRequestsFor = pullRequestsFor' Nothing

-- | A detailed pull request, which has much more information. This takes the
-- repo owner and name along with the number assigned to the pull request.
-- | With authentification
--
-- > pullRequest' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" 562
pullRequest' :: Maybe GithubAuth -> String -> String -> Int -> IO (Either Error DetailedPullRequest)
pullRequest' auth userName reqRepoName number =
  githubGet' auth ["repos", userName, reqRepoName, "pulls", show number]

-- | A detailed pull request, which has much more information. This takes the
-- repo owner and name along with the number assigned to the pull request.
--
-- > pullRequest "thoughtbot" "paperclip" 562
pullRequest :: String -> String -> Int -> IO (Either Error DetailedPullRequest)
pullRequest = pullRequest' Nothing

-- | All the commits on a pull request, given the repo owner, repo name, and
-- the number of the pull request.
-- | With authentification
--
-- > pullRequestCommits' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" 688
pullRequestCommits' :: Maybe GithubAuth -> String -> String -> Int -> IO (Either Error [Commit])
pullRequestCommits' auth userName reqRepoName number =
  githubGet' auth ["repos", userName, reqRepoName, "pulls", show number, "commits"]

-- | All the commits on a pull request, given the repo owner, repo name, and
-- the number of the pull request.
--
-- > pullRequestCommits "thoughtbot" "paperclip" 688
pullRequestCommits :: String -> String -> Int -> IO (Either Error [Commit])
pullRequestCommits = pullRequestCommits' Nothing

-- | The individual files that a pull request patches. Takes the repo owner and
-- name, plus the number assigned to the pull request.
-- | With authentification
--
-- > pullRequestFiles' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" 688
pullRequestFiles' :: Maybe GithubAuth -> String -> String -> Int -> IO (Either Error [File])
pullRequestFiles' auth userName reqRepoName number =
  githubGet' auth ["repos", userName, reqRepoName, "pulls", show number, "files"]
-- | The individual files that a pull request patches. Takes the repo owner and
-- name, plus the number assigned to the pull request.
--
-- > pullRequestFiles "thoughtbot" "paperclip" 688
pullRequestFiles :: String -> String -> Int -> IO (Either Error [File])
pullRequestFiles = pullRequestFiles' Nothing
