{-# LANGUAGE OverloadedStrings #-}
module Github.Data.Teams (
    Permission (..)
  ) where

import           Data.Aeson

import           Data.Text (Text)

data Permission =
    PermissionPull
  | PermissionPush
  | PermissionAdmin
  deriving (Show, Enum, Bounded, Eq, Ord)

-- https://developer.github.com/v3/orgs/teams/#add-or-update-team-repository
renderPermission :: Permission -> Text
renderPermission p =
  case p of
    PermissionPull ->
      "pull"
    PermissionPush ->
      "push"
    PermissionAdmin ->
      "admin"

instance ToJSON Permission where
  toJSON p =
    object [
        "permission" .= renderPermission p
      ]
