-- | The organization members API as described on
-- <http://developer.github.com/v3/orgs/teams/>.
module Github.Organizations.Teams (
 listTeams'
,listTeamMembers'
,module Github.Data
) where

import Github.Data
import Github.Private

listTeams' :: Maybe GithubAuth -> String -> IO (Either Error [Team])
listTeams' auth organization =
  githubGet' auth ["orgs", organization, "teams"]

listTeamMembers' :: Maybe GithubAuth -> Integer -> IO (Either Error [GithubOwner])
listTeamMembers' auth teamId =
  githubGet' auth ["teams", show teamId, "members"]
