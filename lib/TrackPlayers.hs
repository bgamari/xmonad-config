{-# LANGUAGE OverloadedStrings #-}
module TrackPlayers ( trackPlayers
                    , withActivePlayer
                    ) where

import Control.Error
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad
import Control.Concurrent.STM
import Control.Applicative
import Data.List (delete)       
import DBus
import DBus.Client
import MPRIS2

import Control.Concurrent (threadDelay)

trackPlayers :: Client -> EitherT String IO (TVar [Player])
trackPlayers c = do
    players <- findPlayers c >>= liftIO . newTVarIO
    liftIO $ listen c (matchAny {matchMember=Just "NameOwnerChanged"}) (update players)
    return players
  where
    update players signal = void $ runMaybeT $ do
        [name,old,new] <- sequence $ fmap (hoistMaybe . fromVariant) (signalBody signal)
        busName <- hoistMaybe $ parseBusName name
        when (not $ isMPRIS busName) nothing
        let player = Player busName
            update = case (old,new) of
                       ("", _)  -> \names -> player : filter (/= player) names
                       _        -> delete player
        liftIO $ atomically $ modifyTVar players update
  
withActivePlayer :: MonadIO m => TVar [Player] -> (Player -> m ()) -> m ()
withActivePlayer playersVar action = do
    players <- liftIO $ atomically $ readTVar playersVar
    case players of
      []    -> return ()
      p:_   -> action p
      
main = do
    c <- connectSession   
    Right var <- runEitherT (trackPlayers c)
    forever $ do
        atomically (readTVar var) >>= print
        threadDelay (10*1000*1000)

    
