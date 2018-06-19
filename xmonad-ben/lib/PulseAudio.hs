{-# LANGUAGE OverloadedStrings #-}

module PulseAudio ( PAConn
                  , connect
                    -- * Streams
                  , PlaybackStream
                  , getStreams
                    -- * Devices
                  , Device
                  , getDevices
                  , getDeviceName
                  , getDeviceMute
                  , setDeviceMute
                  , toggleDeviceMute
                  , getDeviceVolume
                  , setDeviceVolume
                  , adjustDeviceVolume
                    -- * Volume
                  , Volume(..)
                  , mulVolume
                  ) where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad (join, void)
import Control.Monad.IO.Class
import Control.Error
import Data.Word

import DBus
import DBus.Client (Client, call)
import qualified DBus.Client as C
import Property

newtype PAConn = PAConn Client

lookupBusAddress :: ExceptT String IO Address
lookupBusAddress = do
    session <- liftIO $ C.connectSession
    --ret <- fmapLT show $ ExceptT $ call session c
    ret <- getProperty session "org.PulseAudio1" "/org/pulseaudio/server_lookup1" "org.PulseAudio.ServerLookup1" "Address"
    --case join $ fmap fromVariant $ fromVariant (methodReturnBody ret !! 0) of
    case fromVariant ret of
      Nothing   -> throwE "Error looking up PulseAudio bus"
      Just addr -> parseAddress addr ?? "Invalid address"
  where
    c = (methodCall "/org/pulseaudio/server_lookup1" "org.freedesktop.DBus.Properties" "Get")
        { methodCallDestination = Just "org.PulseAudio1"
        , methodCallBody = [ toVariant ("org.PulseAudio.ServerLookup1" :: String)
                           , toVariant ("Address" :: String)
                           ]
        }

connect :: ExceptT String IO PAConn
connect = do 
    bus <- lookupBusAddress
    PAConn <$> liftIO (C.connect bus)

newtype PlaybackStream = PlaybackStream ObjectPath
                       deriving (Show)

getStreams :: PAConn -> ExceptT String IO [PlaybackStream]
getStreams (PAConn client) = do
    ret <- getProperty client "org.PulseAudio" "/org/pulseaudio/core1" "org.PulseAudio.Core1" "PlaybackStreams"
    streams <- fromVariant ret ?? "Invalid response"
    return $ fmap PlaybackStream streams

newtype Device = Device ObjectPath
               deriving (Show)

getDevices :: PAConn -> ExceptT String IO [Device]
getDevices (PAConn client) = do
    ret <- getProperty client "org.PulseAudio" "/org/pulseaudio/core1" "org.PulseAudio.Core1" "Sinks"
    sinks <- fromVariant ret ?? "Invalid response"
    return $ fmap Device sinks

data Volume = UniformVolume Word32
            | Volume [Word32]
            deriving (Show)

mulVolume :: RealFrac a => a -> Volume -> Volume
mulVolume a x =
    case x of
      UniformVolume v  -> UniformVolume (f v)
      Volume vs        -> Volume $ map f vs
  where
    f = round . (*a) . realToFrac

getDeviceName :: PAConn -> Device -> ExceptT String IO String
getDeviceName (PAConn client) (Device dev) = do
    ret <- getProperty client "org.PulseAudio" dev "org.PulseAudio.Core1.Device" "Name"
    fromVariant ret ?? "Invalid response"

getDeviceMute :: PAConn -> Device -> ExceptT String IO Bool
getDeviceMute (PAConn client) (Device dev) = do
    ret <- getProperty client "org.PulseAudio" dev "org.PulseAudio.Core1.Device" "Mute"
    fromVariant ret ?? "Invalid response"

setDeviceMute :: PAConn -> Device -> Bool -> ExceptT String IO ()
setDeviceMute (PAConn client) (Device dev) mute = do
    setProperty client "org.PulseAudio" dev "org.PulseAudio.Core1.Device" "Mute" (toVariant mute)

toggleDeviceMute :: PAConn -> Device -> ExceptT String IO ()
toggleDeviceMute c d = getDeviceMute c d >>= setDeviceMute c d . not

getDeviceVolume :: PAConn -> Device -> ExceptT String IO Volume
getDeviceVolume (PAConn client) (Device dev) = do
    ret <- getProperty client "org.PulseAudio" dev "org.PulseAudio.Core1.Device" "Volume"
    Volume <$> fromVariant ret ?? "Invalid response"

setDeviceVolume :: PAConn -> Device -> Volume -> ExceptT String IO ()
setDeviceVolume (PAConn client) (Device dev) volume = do
    setProperty client "org.PulseAudio" dev "org.PulseAudio.Core1.Device" "Volume" v
  where
    v = case volume of
            Volume vs       -> toVariant vs
            UniformVolume v -> toVariant [v]

adjustDeviceVolume :: PAConn -> Device -> (Volume -> Volume) -> ExceptT String IO ()
adjustDeviceVolume c dev f = getDeviceVolume c dev >>= setDeviceVolume c dev . f

main :: IO ()
main = runExceptT main' >>= print

main' = do
    c <- connect
    getStreams c >>= liftIO . print
    devices <- getDevices c
    let d:_ = devices
    v <- getDeviceVolume c d
    getDeviceName c d >>= liftIO . print
    setDeviceVolume c d (mulVolume 1.1 v)
    getDeviceMute c d >>= liftIO . print
    setDeviceMute c d True
    setDeviceMute c d False
    return ()
