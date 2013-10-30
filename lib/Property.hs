{-# LANGUAGE OverloadedStrings #-}                

module Property ( -- * Properties
                  PropertyName
                , getProperty
                , setProperty
                  -- * Utilities
                , safeCall
                , safeCall_
                ) where

import DBus
import DBus.Client (Client, call)
import Data.String (IsString(..))
import Control.Error
import Control.Monad (void)

newtype PropertyName = PropertyName String
                     deriving (Show)
        
instance IsString PropertyName where fromString  = PropertyName
    
getProperty :: Client -> BusName -> ObjectPath -> InterfaceName -> PropertyName
            -> EitherT String IO Variant
getProperty client dest path iface (PropertyName prop) = do
    ret <- safeCall client c
    fromVariant ret ?? "Invalid return type"
  where
    c = (methodCall path "org.freedesktop.DBus.Properties" "Get")
        { methodCallDestination = Just dest
        , methodCallBody = [ toVariant iface, toVariant prop ]
        }

setProperty :: Client -> BusName -> ObjectPath -> InterfaceName -> PropertyName
            -> Variant -> EitherT String IO ()
setProperty client dest path iface (PropertyName prop) value = do
    void $ safeCall_ client c
  where
    c = (methodCall path "org.freedesktop.DBus.Properties" "Set")
        { methodCallDestination = Just dest
        , methodCallBody = [ toVariant iface, toVariant prop, toVariant value ]
        }

safeCall_ :: Client -> MethodCall -> EitherT String IO ()
safeCall_ client mcall = do
    void $ fmapLT show $ tryIO $ call client mcall
          
safeCall :: Client -> MethodCall -> EitherT String IO Variant
safeCall client mcall = do
    ret <- fmapLT show $ tryIO $ call client mcall
    either (left . show) (tryHead "Empty response" . methodReturnBody) ret
