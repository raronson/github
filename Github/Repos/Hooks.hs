{-# LANGUAGE OverloadedStrings #-}
-- | The repo starring API as described on
-- <http://developer.github.com/v3/repos/hooks/>.
module Github.Repos.Hooks (
 hooksFor
,singleHook
,createHook
,testHook
,module Github.Data
) where

import Github.Data
import Github.Private

import Data.Aeson (Value(String, Bool, Array), toJSON)
import qualified Data.Map as M
import Data.Text (pack, Text)
import qualified Data.Vector as V

hooksFor :: GithubAuth -> String -> String -> IO (Either Error [Hook])
hooksFor auth userName reqRepoName =
  githubGet' (Just auth) ["repos", userName, reqRepoName, "hooks"]

singleHook :: GithubAuth -> String -> String -> Integer -> IO (Either Error Hook)
singleHook auth userName reqRepoName hid =
  githubGet' (Just auth) ["repos", userName, reqRepoName, "hooks", show hid]

createHook :: GithubAuth -> String -> String -> String -> M.Map String String -> Maybe [String] -> Maybe Bool -> IO (Either Error Hook)
createHook auth userName reqRepoName hookName hookConfig hookEvents hookActive =
  githubPost auth ["repos", userName, reqRepoName, "hooks"]
  $  M.fromList $ ("name", String (pack hookName))
      : ("config", toJSON hookConfig)
      : concat
    [ jinA "events" hookEvents
    , jinB "active" hookActive
    ]

-- Edit a hook: TODO

testHook :: GithubAuth -> String -> String -> Integer -> IO (Either Error ())
testHook auth userName reqRepoName hid =
  githubPost auth ["repos", userName, reqRepoName, "hooks", show hid] ()

-- deleteHook :: GithubAuth -> String -> String -> Integer -> IO (Either Error ())
-- deleteHook auth userName reqRepoName hid =
--   githubDelete auth ["repos", userName, reqRepoName, "hooks", show hid] ()

-- json kludge
jinA :: String -> Maybe [String] -> [(Text, Value)]
jinA k (Just x) = [(pack k, toJSON x)]
jinA _ Nothing = []

jinS :: String -> Maybe String -> [(Text, Value)]
jinS k (Just x) = [(pack k, String (pack x))]
jinS _ Nothing = []

jinB :: String -> Maybe Bool -> [(Text, Value)]
jinB k (Just x) = [(pack k, Bool x)]
jinB _ Nothing = []
