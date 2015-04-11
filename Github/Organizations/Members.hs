-- | The organization members API as described on
-- <http://developer.github.com/v3/orgs/members/>.
module Github.Organizations.Members (
 membersOf
,membersOf'
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All the users who are members of the specified organization.
--
-- > membersOf "thoughtbot"
membersOf :: String -> IO (Either Error [GithubOwner])
membersOf organization = githubGet ["orgs", organization, "members"]

membersOf' :: Maybe GithubAuth -> String -> IO (Either Error [GithubOwner])
membersOf' auth organization = githubGet' auth ["orgs", organization, "members"]
