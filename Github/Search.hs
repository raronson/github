-- | The Github Search API, as described at
-- <http://developer.github.com/v3/search/>.
module Github.Search(
 searchIssues'
,searchIssues
,searchRepos'
,searchRepos
,module Github.Data
) where

import Github.Data
import Github.Private

-- | Perform a repository search.
-- | With authentication.
--
-- > searchRepos' (Just $ GithubBasicAuth "github-username" "github-password') "q=a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
searchRepos' :: Maybe GithubAuth -> String -> IO (Either Error SearchReposResult)
searchRepos' auth queryString = githubGetWithQueryString' auth ["search/repositories"] queryString

-- | Perform a repository search.
-- | Without authentication.
--
-- > searchRepos "q=a in%3Aname language%3Ahaskell created%3A>2013-10-01&per_page=100"
searchRepos :: String -> IO (Either Error SearchReposResult)
searchRepos = searchRepos' Nothing 

-- | Perform an issue search.
-- | With authentication.
--
-- > searchIssues' (Just $ GithubBasicAuth "github-username" "github-password') "q=is%3Aopen"
searchIssues' :: Maybe GithubAuth -> String -> IO (Either Error SearchIssuesResult)
searchIssues' auth queryString = githubGetWithQueryString' auth ["search/issues"] queryString

-- | Perform an issue search.
-- | Without authentication.
--
-- > searchIssues "q=is%3Aopen"
searchIssues :: String -> IO (Either Error SearchIssuesResult)
searchIssues = searchIssues' Nothing 

