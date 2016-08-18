{-# LANGUAGE OverloadedStrings #-}
-- | The repo starring API as described on
-- <http://developer.github.com/v3/repos/hooks/>.
module Github.Repos.Branches (
  ) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS

import           Github.Data
import           Github.Private

import qualified Network.HTTP.Conduit as C (responseStatus)
import qualified Network.HTTP.Types as T (statusCode)


-- https://developer.github.com/v3/repos/branches/#update-branch-protection
--protect :: GithubAuth -> String -> String -> String -> Protection -> IO (Either SomeException ())
protect auth userName reqRepoName branch protection = do
  githubPutBody
    auth
    ["repos", userName, reqRepoName, "branches", branch, "protection"]
    protection

instance ToJSON Protection where
  toJSON (Protection r p) =
    object [
        ("required_status_checks", maybeOrNull r $ \(RequiredStatusChecks e s c) ->
          object [
              "include_admins" .= case e of { Everyone -> True; NotAdmins -> False }
            , "strict" .= s
            , "contexts" .= toJSON c
            ])
      , ("restrictions", maybeOrNull p $ \(PushRestrictions us ts) ->
          object [
              "users" .= toJSON (fmap user us)
            , "teams" .= toJSON (fmap team ts)
            ])
      ]

maybeOrNull :: Maybe a -> (a -> Value) -> Value
maybeOrNull m f =
  case m of
    Nothing ->
      Null
    Just a ->
      f a
