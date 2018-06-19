{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WorkspacePrompt ( selectWorkspace ) where

import Data.Monoid
import Data.List (sortBy, isInfixOf, isPrefixOf)
import Data.List.Split (splitWhen)
import Data.Char (isAlphaNum)

import XMonad hiding ( workspaces )
import XMonad.Prompt
import XMonad.StackSet hiding (filter, modify, delete)
import XMonad.Util.WorkspaceCompare ( getSortByIndex )
import XMonad.Prompt.Workspace (Wor(..))
import XMonad.Actions.DynamicWorkspaces (addWorkspace)

selectWorkspace :: XPConfig -> X ()
selectWorkspace conf = workspacePrompt conf $ \w -> do
    s <- gets windowset
    if tagMember w s
        then windows $ greedyView w
        else addWorkspace w

workspacePrompt :: XPConfig -> (String -> X ()) -> X ()
workspacePrompt c job = do
    ws <- gets (workspaces . windowset)
    sort <- getSortByIndex
    let ts = map tag $ sort ws
    mkXPrompt (Wor "") c (pure . mkHelmComplFunFromList ts) job

-- | A completion function which roughly emulates Helm's matching functionality.
mkHelmComplFunFromList :: [String] -> String -> [String]
mkHelmComplFunFromList compls = \query ->
    let queryParts = words query
        completionScore compl =
            let isInfixMatch queryPart =
                    queryPart `isInfixOf` compl
                prefixMatch queryPart =
                    foldMap (\complPart -> scoreOf (Score 2) $ queryPart `isPrefixOf` complPart) complParts
                  where
                    complParts = splitWhen (not . isAlphaNum) compl

            in if all isInfixMatch queryParts
                   then foldMap (scoreOf (Score 1) . isInfixMatch) queryParts <> foldMap prefixMatch queryParts
                   else mempty
    in map snd
       $ sortBy compare
       $ filter (\(s,_) -> s /= mempty)
       $ map (\x -> (completionScore x, x)) compls

newtype Score = Score Int
              deriving (Eq)

instance Monoid Score where
    mempty = Score 0
    Score x `mappend` Score y = Score (x + y)

instance Ord Score where
    Score x `compare` Score y = y `compare` x

scoreOf :: Score -> Bool -> Score
scoreOf score True  = score
scoreOf _     False = mempty

testWorkspaces = ["mail", "irc", "haddock", "music", "wt-dwarf", "wt-ghc-8", "wt-ghc-land", "wt-omn", "wt-omn-chat", "wt-omn-ssh", "wt-osx", "wt-status", "wt-todo", "xmonad"]
