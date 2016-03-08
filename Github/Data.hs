{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

-- | This module re-exports the @Github.Data.Definitions@ module, adding
-- instances of @FromJSON@ to it. If you wish to use the data without the
-- instances, use the @Github.Data.Definitions@ module instead.

module Github.Data (module X) where

import Data.Time
import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import Data.Aeson.Types
import System.Locale (defaultTimeLocale)
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as Map
import Data.Hashable (Hashable)

import Github.Data.Definitions as X
import Github.Data.Teams as X

instance FromJSON GithubDate where
  parseJSON (String t) =
    case parseTime defaultTimeLocale "%FT%T%Z" (T.unpack t) of
         Just d -> pure $ GithubDate d
         _      -> fail "could not parse Github datetime"
  parseJSON _          = fail "Given something besides a String"

instance FromJSON Commit where
  parseJSON (Object o) =
    Commit <$> o .: "sha"
           <*> o .: "parents"
           <*> o .: "url"
           <*> o .: "commit"
           <*> o .:? "committer"
           <*> o .:? "author"
           <*> o .:< "files"
           <*> o .:? "stats"
  parseJSON _          = fail "Could not build a Commit"

instance FromJSON Tree where
  parseJSON (Object o) =
    Tree <$> o .: "sha"
         <*> o .: "url"
         <*> o .:< "tree"
  parseJSON _          = fail "Could not build a Tree"

instance FromJSON GitTree where
  parseJSON (Object o) =
    GitTree <$> o .: "type"
         <*> o .: "sha"
         <*> o .:? "url"
         <*> o .:? "size"
         <*> o .: "path"
         <*> o .: "mode"
  parseJSON _          = fail "Could not build a GitTree"

instance FromJSON GitCommit where
  parseJSON (Object o) =
    GitCommit <$> o .: "message"
              <*> o .: "url"
              <*> o .: "committer"
              <*> o .: "author"
              <*> o .: "tree"
              <*> o .:? "sha"
              <*> o .:< "parents"
  parseJSON _          = fail "Could not build a GitCommit"

instance FromJSON GithubOwner where
  parseJSON (Object o)
    | o `at` "gravatar_id" == Nothing =
      GithubOrganization <$> o .: "avatar_url"
                 <*> o .: "login"
                 <*> o .: "url"
                 <*> o .: "id"
    | otherwise =
      GithubUser <$> o .: "avatar_url"
                 <*> o .: "login"
                 <*> o .: "url"
                 <*> o .: "id"
                 <*> o .: "gravatar_id"
  parseJSON v          = fail $ "Could not build a GithubOwner out of " ++ (show v)

instance FromJSON GitUser where
  parseJSON (Object o) =
    GitUser <$> o .: "name"
            <*> o .: "email"
            <*> o .: "date"
  parseJSON _          = fail "Could not build a GitUser"

instance FromJSON File where
  parseJSON (Object o) =
    File <$> o .: "blob_url"
         <*> o .: "status"
         <*> o .: "raw_url"
         <*> o .: "additions"
         <*> o .: "sha"
         <*> o .: "changes"
         <*> o .: "patch"
         <*> o .: "filename"
         <*> o .: "deletions"
  parseJSON _          = fail "Could not build a File"

instance FromJSON Stats where
  parseJSON (Object o) =
    Stats <$> o .: "additions"
          <*> o .: "total"
          <*> o .: "deletions"
  parseJSON _          = fail "Could not build a Stats"

instance FromJSON Comment where
  parseJSON (Object o) =
    Comment <$> o .:? "position"
            <*> o .:? "line"
            <*> o .: "body"
            <*> o .: "commit_id"
            <*> o .: "updated_at"
            <*> o .:? "html_url"
            <*> o .: "url"
            <*> o .: "created_at"
            <*> o .: "path"
            <*> o .: "user"
            <*> o .: "id"
  parseJSON _          = fail "Could not build a Comment"

instance ToJSON NewComment where
  toJSON (NewComment b) = object [ "body" .= b ]

instance ToJSON EditComment where
  toJSON (EditComment b) = object [ "body" .= b ]

instance FromJSON Diff where
  parseJSON (Object o) =
    Diff <$> o .: "status"
         <*> o .: "behind_by"
         <*> o .: "patch_url"
         <*> o .: "url"
         <*> o .: "base_commit"
         <*> o .:< "commits"
         <*> o .: "total_commits"
         <*> o .: "html_url"
         <*> o .:< "files"
         <*> o .: "ahead_by"
         <*> o .: "diff_url"
         <*> o .: "permalink_url"
  parseJSON _          = fail "Could not build a Diff"

instance FromJSON Gist where
  parseJSON (Object o) =
    Gist <$> o .: "user"
         <*> o .: "git_push_url"
         <*> o .: "url"
         <*> o .:? "description"
         <*> o .: "created_at"
         <*> o .: "public"
         <*> o .: "comments"
         <*> o .: "updated_at"
         <*> o .: "html_url"
         <*> o .: "id"
         <*> o `values` "files"
         <*> o .: "git_push_url"
  parseJSON _          = fail "Could not build a Gist"

instance FromJSON GistFile where
  parseJSON (Object o) =
    GistFile <$> o .: "type"
             <*> o .: "raw_url"
             <*> o .: "size"
             <*> o .:? "language"
             <*> o .: "filename"
             <*> o .:? "content"
  parseJSON _          = fail "Could not build a GistFile"

instance FromJSON GistComment where
  parseJSON (Object o) =
    GistComment <$> o .: "user"
                <*> o .: "url"
                <*> o .: "created_at"
                <*> o .: "body"
                <*> o .: "updated_at"
                <*> o .: "id"
  parseJSON _          = fail "Could not build a GistComment"

instance FromJSON Blob where
  parseJSON (Object o) =
    Blob <$> o .: "url"
         <*> o .: "encoding"
         <*> o .: "content"
         <*> o .: "sha"
         <*> o .: "size"
  parseJSON _          = fail "Could not build a Blob"

instance FromJSON GitReference where
  parseJSON (Object o) =
    GitReference <$> o .: "object"
                 <*> o .: "url"
                 <*> o .: "ref"
  parseJSON _          = fail "Could not build a GitReference"

instance FromJSON GitObject where
  parseJSON (Object o) =
    GitObject <$> o .: "type"
           <*> o .: "sha"
           <*> o .: "url"
  parseJSON _          = fail "Could not build a GitObject"

instance FromJSON Issue where
  parseJSON (Object o) =
    Issue <$> o .:? "closed_at"
          <*> o .: "updated_at"
          <*> o .: "html_url"
          <*> o .:? "closed_by"
          <*> o .: "labels"
          <*> o .: "number"
          <*> o .:? "assignee"
          <*> o .: "user"
          <*> o .: "title"
          <*> o .:? "pull_request"
          <*> o .: "url"
          <*> o .: "created_at"
          <*> o .: "body"
          <*> o .: "state"
          <*> o .: "id"
          <*> o .: "comments"
          <*> o .:? "milestone"
  parseJSON _          = fail "Could not build an Issue"

instance ToJSON NewIssue where
  toJSON (NewIssue t b a m ls) =
    object
    [ "title"     .= t
    , "body"      .= b
    , "assignee"  .= a
    , "milestone" .= m
    , "labels"    .= ls ]

instance ToJSON EditIssue where
  toJSON (EditIssue t b a s m ls) =
    object $ filter notNull $ [ "title" .= t
                              , "body" .= b
                              , "assignee" .= a
                              , "state" .= s
                              , "milestone" .= m
                              , "labels" .= ls ]
    where notNull (_, Null) = False
          notNull (_, _)    = True

instance FromJSON Milestone where
  parseJSON (Object o) =
    Milestone <$> o .: "creator"
              <*> o .: "due_on"
              <*> o .: "open_issues"
              <*> o .: "number"
              <*> o .: "closed_issues"
              <*> o .: "description"
              <*> o .: "title"
              <*> o .: "url"
              <*> o .: "created_at"
              <*> o .: "state"
  parseJSON _          = fail "Could not build a Milestone"

instance FromJSON IssueLabel where
  parseJSON (Object o) =
    IssueLabel <$> o .: "color"
               <*> o .: "url"
               <*> o .: "name"
  parseJSON _          = fail "Could not build a Milestone"

instance FromJSON PullRequestReference where
  parseJSON (Object o) =
    PullRequestReference <$> o .:? "html_url"
                         <*> o .:? "patch_url"
                         <*> o .:? "diff_url"
  parseJSON _          = fail "Could not build a PullRequest"

instance FromJSON IssueComment where
  parseJSON (Object o) =
    IssueComment <$> o .: "updated_at"
                 <*> o .: "user"
                 <*> o .: "url"
                 <*> o .: "created_at"
                 <*> o .: "body"
                 <*> o .: "id"
  parseJSON _          = fail "Could not build an IssueComment"

instance FromJSON Event where
  parseJSON (Object o) =
    Event <$> o .: "actor"
          <*> o .: "event"
          <*> o .:? "commit_id"
          <*> o .: "url"
          <*> o .: "created_at"
          <*> o .: "id"
          <*> o .:? "issue"
  parseJSON _          = fail "Could not build an Event"

instance FromJSON EventType where
  parseJSON (String "closed") = pure Closed
  parseJSON (String "reopened") = pure Reopened
  parseJSON (String "subscribed") = pure Subscribed
  parseJSON (String "merged") = pure Merged
  parseJSON (String "referenced") = pure Referenced
  parseJSON (String "mentioned") = pure Mentioned
  parseJSON (String "assigned") = pure Assigned
  parseJSON (String "unsubscribed") = pure Unsubscribed
  parseJSON _ = fail "Could not build an EventType"

instance FromJSON SimpleOrganization where
  parseJSON (Object o) =
    SimpleOrganization <$> o .: "url"
                       <*> o .: "avatar_url"
                       <*> o .: "id"
                       <*> o .: "login"
  parseJSON _ = fail "Could not build a SimpleOrganization"

instance FromJSON Organization where
  parseJSON (Object o) =
    Organization <$> o .: "type"
                 <*> o .:? "blog"
                 <*> o .:? "location"
                 <*> o .: "login"
                 <*> o .: "followers"
                 <*> o .:? "company"
                 <*> o .: "avatar_url"
                 <*> o .: "public_gists"
                 <*> o .: "html_url"
                 <*> o .:? "email"
                 <*> o .: "following"
                 <*> o .: "public_repos"
                 <*> o .: "url"
                 <*> o .: "created_at"
                 <*> o .:? "name"
                 <*> o .: "id"
  parseJSON _ = fail "Could not build an Organization"

instance FromJSON PullRequest where
  parseJSON (Object o) =
      PullRequest
        <$> o .:? "closed_at"
        <*> o .: "created_at"
        <*> o .: "user"
        <*> o .: "patch_url"
        <*> o .: "state"
        <*> o .: "number"
        <*> o .: "html_url"
        <*> o .: "updated_at"
        <*> o .: "body"
        <*> o .: "issue_url"
        <*> o .: "diff_url"
        <*> o .: "url"
        <*> o .: "_links"
        <*> o .:? "merged_at"
        <*> o .: "title"
        <*> o .: "id"
  parseJSON _ = fail "Could not build a PullRequest"

instance FromJSON DetailedPullRequest where
  parseJSON (Object o) =
      DetailedPullRequest
        <$> o .:? "closed_at"
        <*> o .: "created_at"
        <*> o .: "user"
        <*> o .: "patch_url"
        <*> o .: "state"
        <*> o .: "number"
        <*> o .: "html_url"
        <*> o .: "updated_at"
        <*> o .: "body"
        <*> o .: "issue_url"
        <*> o .: "diff_url"
        <*> o .: "url"
        <*> o .: "_links"
        <*> o .:? "merged_at"
        <*> o .: "title"
        <*> o .: "id"
        <*> o .:? "merged_by"
        <*> o .: "changed_files"
        <*> o .: "head"
        <*> o .: "comments"
        <*> o .: "deletions"
        <*> o .: "additions"
        <*> o .: "review_comments"
        <*> o .: "base"
        <*> o .: "commits"
        <*> o .: "merged"
        <*> o .: "mergeable"
  parseJSON _ = fail "Could not build a DetailedPullRequest"

instance FromJSON PullRequestLinks where
  parseJSON (Object o) =
    PullRequestLinks <$> o <.:> ["review_comments", "href"]
                     <*> o <.:> ["comments", "href"]
                     <*> o <.:> ["html", "href"]
                     <*> o <.:> ["self", "href"]
  parseJSON _ = fail "Could not build a PullRequestLinks"

instance FromJSON PullRequestCommit where
  parseJSON (Object _) =
    return PullRequestCommit
  parseJSON _ = fail "Could not build a PullRequestCommit"

instance FromJSON SearchReposResult where
  parseJSON (Object o) =
    SearchReposResult <$> o .: "total_count"
                      <*> o .:< "items"
  parseJSON _ = fail "Could not build a SearchReposResult"

instance FromJSON Repo where
  parseJSON (Object o) =
    Repo <$> o .: "ssh_url"
         <*> o .: "description"
         <*> o .: "created_at"
         <*> o .: "html_url"
         <*> o .: "svn_url"
         <*> o .: "forks"
         <*> o .:? "homepage"
         <*> o .: "fork"
         <*> o .: "git_url"
         <*> o .: "private"
         <*> o .: "clone_url"
         <*> o .: "size"
         <*> o .: "updated_at"
         <*> o .: "watchers"
         <*> o .: "owner"
         <*> o .: "name"
         <*> o .: "language"
         <*> o .:? "master_branch"
         <*> o .: "pushed_at"
         <*> o .: "id"
         <*> o .: "url"
         <*> o .: "open_issues"
         <*> o .:? "has_wiki"
         <*> o .:? "has_issues"
         <*> o .:? "has_downloads"
	 <*> o .:? "parent"
	 <*> o .:? "source"
  parseJSON _ = fail "Could not build a Repo"

instance FromJSON RepoRef where
  parseJSON (Object o) =
    RepoRef <$> o .: "owner"
            <*> o .: "name"
  parseJSON _ = fail "Could not build a RepoRef"

instance FromJSON Contributor where
  parseJSON (Object o)
    | o `at` "type" == (Just "Anonymous") =
      AnonymousContributor <$> o .: "contributions"
                           <*> o .: "name"
    | otherwise =
      KnownContributor <$> o .: "contributions"
                       <*> o .: "avatar_url"
                       <*> o .: "login"
                       <*> o .: "url"
                       <*> o .: "id"
                       <*> o .: "gravatar_id"
  parseJSON _ = fail "Could not build a Contributor"

instance FromJSON Languages where
  parseJSON (Object o) =
    Languages <$>
      mapM (\name -> Language (T.unpack name) <$> o .: name)
           (Map.keys o)
  parseJSON _ = fail "Could not build Languages"

instance FromJSON Tag where
  parseJSON (Object o) =
    Tag <$> o .: "name"
        <*> o .: "zipball_url"
        <*> o .: "tarball_url"
        <*> o .: "commit"
  parseJSON _ = fail "Could not build a Tag"

instance FromJSON Branch where
  parseJSON (Object o) = Branch <$> o .: "name" <*> o .: "commit"
  parseJSON _ = fail "Could not build a Branch"

instance FromJSON BranchCommit where
  parseJSON (Object o) = BranchCommit <$> o .: "sha" <*> o .: "url"
  parseJSON _ = fail "Could not build a BranchCommit"

instance FromJSON DetailedOwner where
  parseJSON (Object o)
    | o `at` "gravatar_id" == Nothing =
      DetailedOrganization <$> o .: "created_at"
                   <*> o .: "type"
                   <*> o .: "public_gists"
                   <*> o .: "avatar_url"
                   <*> o .: "followers"
                   <*> o .: "following"
                   <*> o .:? "blog"
                   <*> o .:? "bio"
                   <*> o .: "public_repos"
                   <*> o .:? "name"
                   <*> o .:? "location"
                   <*> o .:? "company"
                   <*> o .: "url"
                   <*> o .: "id"
                   <*> o .: "html_url"
                   <*> o .: "login"
    | otherwise =
      DetailedUser <$> o .: "created_at"
                   <*> o .: "type"
                   <*> o .: "public_gists"
                   <*> o .: "avatar_url"
                   <*> o .: "followers"
                   <*> o .: "following"
                   <*> o .: "hireable"
                   <*> o .: "gravatar_id"
                   <*> o .:? "blog"
                   <*> o .:? "bio"
                   <*> o .: "public_repos"
                   <*> o .:? "name"
                   <*> o .:? "location"
                   <*> o .:? "company"
                   <*> o .: "email"
                   <*> o .: "url"
                   <*> o .: "id"
                   <*> o .: "html_url"
                   <*> o .: "login"
  parseJSON _ = fail "Could not build a DetailedOwner"


instance FromJSON Hook where
  parseJSON (Object o) = Hook <$> o .: "id"
                              <*> o .: "config"
                              <*> o .: "active"
                              <*> o .: "events"
                              <*> o .: "name"
                              <*> o .: "created_at"
                              <*> o .: "updated_at"
  parseJSON _ = fail "Could not build a Hook"

-- | A slightly more generic version of Aeson's @(.:?)@, using `mzero' instead
-- of `Nothing'.
(.:<) :: (FromJSON a) => Object -> T.Text -> Parser [a]
obj .:< key = case Map.lookup key obj of
                   Nothing -> pure mzero
                   Just v  -> parseJSON v

-- | Produce all values for the given key.
values :: (Eq k, Hashable k, FromJSON v) => Map.HashMap k Value -> k -> Parser v
obj `values` key =
  let (Object children) = findWithDefault (Object Map.empty) key obj in
    parseJSON $ Array $ V.fromList $ Map.elems children

-- | Produce the value for the last key by traversing.
(<.:>) :: (FromJSON v) => Object -> [T.Text] -> Parser v
obj <.:> [key] = obj .: key
obj <.:> (key:keys) =
  let (Object nextObj) = findWithDefault (Object Map.empty) key obj in
      nextObj <.:> keys
_ <.:> [] = fail "must have a pair"

-- | Produce the value for the given key, maybe.
at :: Object -> T.Text -> Maybe Value
obj `at` key = Map.lookup key obj

-- Taken from Data.Map:
findWithDefault :: (Eq k, Hashable k) => v -> k -> Map.HashMap k v -> v
findWithDefault def k m =
  case Map.lookup k m of
    Nothing -> def
    Just x  -> x
