#!/usr/bin/env stack
{- stack
  script
  --resolver lts-15.2
  --package "directory typed-process fdo-notify http-conduit aeson unordered-containers text mtl lens lens-aeson"
-}

{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Process.Typed (runProcess, runProcess_, readProcess, shell)
import Control.Exception (IOException, catch)
import Data.List (elemIndex)
import Control.Monad (void)
import Network.HTTP.Simple (httpJSON, getResponseBody)
import Data.Aeson
import qualified Data.Text as T (Text)
import DBus.Notify 
import Control.Monad.Except (ExceptT, runExceptT, lift, throwError)
--import Control.Concurrent (threadDelay)
import Control.Lens ((^?))
import Data.Aeson.Lens (key, _String)

data Vpn = Off | On String
type VpnToggle = ExceptT String IO Vpn

data Notify = CLI | Desktop

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["desk"] -> runExceptT (run []) >>= outputResult Desktop
    [arg, "desk"] -> runExceptT (run [arg]) >>= outputResult Desktop
    other -> runExceptT (run other) >>= outputResult CLI

outputResult :: Notify -> Either String Vpn -> IO ()
outputResult CLI (Right (On server)) = getLocation >>= putStrLn . locationString server . show
outputResult CLI (Right Off) = putStrLn "Vpn is stopped" -- maybe get location too?
outputResult CLI (Left e) = putStrLn e
outputResult Desktop (Right (On server)) = getLocation >>= notifyDesktop . locationString server . show
outputResult Desktop (Right Off) = notifyDesktop "Vpn is stopped"
outputResult Desktop (Left e) = notifyDesktop e

locationString :: String -> String -> String
locationString server location = "Vpn is started. Server: " <> server <> ". Location: " <> init (tail location)

getLocation :: IO T.Text
getLocation = do
  response <- getResponseBody <$> httpJSON "http://ifconfig.co/json" :: IO Value
  case response ^? key "country" . _String of
    Just c -> return c
    _ -> error "Error: something unexpected in the response from getting location"

notifyDesktop :: String -> IO ()
notifyDesktop message = do
  client <- connectSession
  void $ notify client $ blankNote {summary = "VPN",
                                    body = Just $ Text message}

run :: [String] -> VpnToggle
run [] = start defaultServer
run ["location"] = currentVpn
run ["next"] = next
run ["stop"] = stop
run [server] = if server `elem` servers
                 then start server
                 else throwError "No such vpn server"
run _ = throwError "Too many arguments"

start :: String -> VpnToggle  
start server = do
  current <- currentVpn
  case current of
    On c -> throwError $ "Vpn is already running: " <> c
    Off -> do
      stat <- status server
      case stat of
        Off -> lift $ do
          void $ runProcess $ shell $ "sudo /etc/init.d/openvpn." <> server <> " start"
          createDirectoryIfMissing False "/tmp/vpn" -- add checking status of just started vpn?
          writeFile "/tmp/vpn/current" server
          return (On server)
        On _ -> throwError "Error: running vpn is not in tmp file"

next :: VpnToggle
next = do
  current <- currentVpn
  case current of
    On c -> do void stop
               On server <- nextInList c -- ugly
               start server
    Off -> throwError "Vpn is not running"

stop :: VpnToggle
stop = do
  current <- currentVpn
  case current of
    On server -> lift $ do
      runProcess_ $ shell $ "sudo /etc/init.d/openvpn." <> server <> " stop" 
      removeFile "/tmp/vpn/current" -- add checking status of just stopped?
      return Off
    Off -> throwError "Vpn is not running"

currentVpn :: VpnToggle
currentVpn = do
  vpnInTmp <- lift $ fmap On (readFile "/tmp/vpn/current") `catch` exceptionHandler
  case vpnInTmp of
    On server -> do
      vpnStatus <- status server
      case vpnStatus of
        Off -> throwError "Error: current vpn in tmp file is not running"
        on -> return on
    Off -> return Off
  where exceptionHandler :: IOException -> IO Vpn
        exceptionHandler _ = return Off

status :: String -> VpnToggle
status server = do
  (_, out, _) <- lift $ readProcess $ shell $ "sudo /etc/init.d/openvpn." <> server <> " status"
  case out of
    " * status: stopped\n" -> return Off
    " * status: started\n" -> return $ On server
    _ -> throwError "Error: something unexpected in status checking"

servers :: [String]
servers = ["nl1", "nl2", "us1", "us2", "jp1", "jp2", "jp3"]
defaultServer :: String
defaultServer = "nl1"

nextInList :: String -> VpnToggle
nextInList c = case fmap (\i -> servers !! if i + 1 == length servers then 0 else i + 1) (c `elemIndex` servers) of
  Just server -> return (On server) -- ugly
  Nothing -> throwError "Error: something unexpected in finding next vpn in the list"
