{-# LANGUAGE OverloadedStrings #-}
                
module MPRIS2 ( -- * Media players
                Player(..)
              , findPlayers
              , getIdentity
                -- * Actions
              , next
              , previous
              , pause
              , playPause
              , stop
              , play
                -- * Internal utilities
              , isMPRIS
                -- * Convenient re-exports
              , connectSession
              ) where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Error
import Control.Applicative
import Data.List (isPrefixOf)

import DBus
import DBus.Client (connectSession, Client, call)
import Property       

newtype Player = Player BusName
               deriving (Show, Eq, Ord)

listNames :: Client -> EitherT String IO [BusName]
listNames client = do
    rep <- fmapLT show $ EitherT $ call client
           $ (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
             { methodCallDestination = Just "org.freedesktop.DBus" }
    tryHead "Empty response" (methodReturnBody rep)
      >>= tryJust "Invalid response" . fromVariant
      >>= return . map busName_

isMPRIS :: BusName -> Bool
isMPRIS = isPrefixOf "org.mpris.MediaPlayer2." . formatBusName

findPlayers :: Client -> EitherT String IO [Player]
findPlayers c = map Player . filter isMPRIS <$> listNames c

getIdentity :: Client -> Player -> EitherT String IO String
getIdentity c (Player n) = do
    ret <- getProperty c n "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2" "Identity"
    tryJust "identity: Invalid response" $ fromVariant ret

playerAction :: MemberName -> Client -> Player -> EitherT String IO ()
playerAction m c (Player n) = do 
    void $ safeCall_ c (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" m)
                       { methodCallDestination = Just n }

next = playerAction "Next"
previous = playerAction "Previous"
play = playerAction "Play"
pause = playerAction "Pause"     
playPause = playerAction "PlayPause"
stop = playerAction "Stop"

main = do
    c <- connectSession
    runEitherT $ do
        players <- findPlayers c
        mapM (getIdentity c) players >>= liftIO . print
        mapM (playPause c) players

