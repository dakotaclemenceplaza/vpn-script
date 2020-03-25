#!/usr/bin/env stack
{- stack
  script
  --resolver lts-15.2
  --package "directory typed-process fdo-notify http-conduit aeson unordered-containers text mtl lens lens-aeson"
-}

{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Process.Typed (runProcess_, readProcess, shell)
import Control.Exception (IOException, catch)
import Data.List (elemIndex)
import Control.Monad (void)
import Network.HTTP.Simple (httpJSON, getResponseBody)
import Data.Aeson
import qualified Data.Text as T (Text)
import DBus.Notify 
import Control.Monad.Except (ExceptT, runExceptT, lift, throwError)
import Control.Concurrent (threadDelay)
import Control.Lens ((^?))
import Data.Aeson.Lens (key, _String)

servers :: [String]
servers = ["nl1", "nl2", "us1", "us2", "jp1", "jp2", "jp3"]
defaultServer :: String
defaultServer = "nl1"

data Vpn = Off | On String
type VpnToggle a = ExceptT String IO a

data Notify = CLI | Desktop

main :: IO ()
main = do
  args <- getArgs
  end <- case args of
           ["desk"] -> runExceptT $ run Desktop []
           [arg, "desk"] -> runExceptT $ run Desktop [arg]
           other -> runExceptT $ run CLI other
  case end of
    Right _ -> pure ()
    Left e -> putStrLn e -- errors go to stdout only
    
run :: Notify -> [String] -> VpnToggle ()
run notifyMode arg = let outRes = outputResult notifyMode
                     in case arg of
                          [] -> start defaultServer >>= outRes
                          ["location"] -> currentVpn >>= outRes
                          ["next"] -> next >>= outRes
                          ["stop"] -> stop >>= outRes
                          [server] -> if server `elem` servers
                                      then start server >>= outRes
                                      else throwError "No such vpn server"
                          _ -> throwError "Bad arguments"

outputResult :: Notify -> Vpn -> VpnToggle ()
outputResult CLI (On server) = do
  lift $ threadDelay 3000000 -- how long to wait?
  getLocation >>= lift . putStrLn . locationString server . show
outputResult CLI Off = lift $ putStrLn "Vpn is stopped"
outputResult Desktop (On server) = do
  lift $ threadDelay 3000000
  getLocation >>= lift . notifyDesktop . locationString server . show
outputResult Desktop Off = lift $ notifyDesktop "Vpn is stopped"

start :: String -> VpnToggle Vpn
start server = do
  current <- currentVpn
  case current of
    On c -> throwError $ "Vpn is already running: " <> c
    Off -> do
      stat <- status server
      case stat of
        Off -> lift $ do
          runProcess_ $ shell $ "sudo /etc/init.d/openvpn." <> server <> " start"
          createDirectoryIfMissing False "/tmp/vpn" -- add checking status of just started vpn?
          writeFile "/tmp/vpn/current" server
          pure (On server)
        On _ -> throwError "Error: running vpn is not in tmp file"

next :: VpnToggle Vpn
next = do
  current <- currentVpn
  case current of
    On c -> stop >> nextInList c >>= start
    Off -> throwError "Vpn is not running"

stop :: VpnToggle Vpn
stop = do
  current <- currentVpn
  case current of
    On server -> lift $ do
      runProcess_ $ shell $ "sudo /etc/init.d/openvpn." <> server <> " stop" 
      removeFile "/tmp/vpn/current" -- add checking status of just stopped?
      pure Off
    Off -> throwError "Vpn is not running"

currentVpn :: VpnToggle Vpn
currentVpn = do
  vpnInTmp <- lift $ fmap On (readFile "/tmp/vpn/current") `catch` exceptionHandler
  case vpnInTmp of
    On server -> do
      vpnStatus <- status server
      case vpnStatus of
        Off -> throwError "Error: current vpn in tmp file is not running"
        on -> pure on
    Off -> pure Off
  where exceptionHandler :: IOException -> IO Vpn
        exceptionHandler _ = pure Off

status :: String -> VpnToggle Vpn
status server = do
  (_, out, _) <- lift $ readProcess $ shell $ "sudo /etc/init.d/openvpn." <> server <> " status"
  case out of
    " * status: stopped\n" -> pure Off
    " * status: started\n" -> pure $ On server
    _ -> throwError "Error: something unexpected in status checking"

getLocation :: VpnToggle T.Text
getLocation = do
  response <- lift $ getResponseBody <$> httpJSON "http://ifconfig.co/json" :: VpnToggle Value
  case response ^? key "country" . _String of
    Just c -> pure c
    _ -> throwError "Error: something unexpected in the response from getting location"

notifyDesktop :: String -> IO ()
notifyDesktop message = do
  client <- connectSession
  void $ notify client $ blankNote {summary = "VPN",
                                    body = Just $ Text message}

nextInList :: String -> VpnToggle String
nextInList c = case fmap (\i -> servers !! if i + 1 == length servers then 0 else i + 1) (c `elemIndex` servers) of
  Just server -> pure server
  Nothing -> throwError "Error: something unexpected in finding next vpn in the list"

locationString :: String -> String -> String
locationString server location = "Vpn is started. Server: " <> server <> ". Location: " <> init (tail location) <> "."
