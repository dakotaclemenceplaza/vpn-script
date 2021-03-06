#!/usr/bin/env stack
{- stack
  script
  --resolver lts-17.11
  --package "directory typed-process fdo-notify http-conduit aeson unordered-containers text mtl lens lens-aeson"
-}

{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, removeFile, listDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (runProcess, runProcess_, readProcess, shell)
import Control.Exception (IOException, catch)
import Data.List (elemIndex, isSuffixOf)
import Control.Monad (void)
import Network.HTTP.Simple (httpJSON, getResponseBody)
import Data.Aeson (Value)
import qualified Data.Text as T (Text)
import DBus.Notify (summary, body, connectSession, notify, blankNote, Body(Text))
import Control.Monad.Except (ExceptT, runExceptT, liftIO, throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Concurrent (threadDelay)
import Control.Lens ((^?))
import Data.Aeson.Lens (key, _String)

defaultServer :: String
defaultServer = "nl1"

type Server = String
data Vpn = Off | On Server
data Result = V Vpn | Message String
type VpnToggle a = ExceptT String (ReaderT [Server] IO) a

data Notify = CLI | Desktop

main :: IO ()
main = do
  args <- getArgs
  servers <- listDirectory "/etc/openvpn" >>= pure . map (takeWhile (/='.')) . filter (".conf" `isSuffixOf`)
  end <- case args of
           ["desk"] -> runReaderT (runExceptT $ run Desktop []) servers
           [arg, "desk"] -> runReaderT (runExceptT $ run Desktop [arg]) servers
           other -> runReaderT (runExceptT $ run CLI other) servers
  case end of
    Right _ -> pure ()
    Left e -> hPutStrLn stderr e
    
run :: Notify -> [String] -> VpnToggle ()
run notifyMode arg = let outRes = outputResult notifyMode
                     in case arg of
                          [] -> start defaultServer >>= outRes
                          ["location"] -> do
                            vpn <- currentVpn
                            case vpn of
                              On server -> outRes (V $ On server)
                              Off -> outRes (V Off)
                          ["next"] -> next >>= outRes
                          ["stop"] -> stop >>= outRes
                          ["clean"] -> clean >>= outRes
                          [server] -> do
                            servers <- ask
                            if server `elem` servers
                              then start server >>= outRes
                              else outRes $ Message "No such vpn server"
                          _ -> outRes $ Message "Unknown arguments"

outputResult :: Notify -> Result -> VpnToggle ()
outputResult CLI (V (On server)) = do
  liftIO $ threadDelay 3000000 -- how long to wait?
  getLocation >>= liftIO . putStrLn . locationString server . show
outputResult Desktop (V (On server)) = do
  liftIO $ threadDelay 3000000
  getLocation >>= liftIO . notifyDesktop . locationString server . show
outputResult CLI (V Off) = liftIO $ putStrLn "Vpn is stopped"
outputResult CLI (Message m) = liftIO $ putStrLn m
outputResult Desktop (V Off) = liftIO $ notifyDesktop "Vpn is stopped"
outputResult Desktop (Message m) = liftIO $ notifyDesktop m

start :: String -> VpnToggle Result
start server = do
  current <- currentVpn
  case current of
    On c -> pure $ Message $ "Vpn is already running: " <> c
    Off -> do
      stat <- status server
      case stat of
        Off -> liftIO $ do
          -- next line returns error exit code but it works fine, so use runProcess and
          -- throw away exit code instead of runProcess_ which checks it and rethrows
          void $ runProcess $ shell $ "sudo /etc/init.d/openvpn." <> server <> " start"
          createDirectoryIfMissing False "/tmp/vpn" -- add checking status of just started vpn?
          writeFile "/tmp/vpn/current" server
          pure $ V $ On server
        On _ -> throwError "Error: running vpn is not in tmp file"
    
next :: VpnToggle Result
next = do
  current <- currentVpn
  case current of
    On c -> stop >> nextInList c >>= start
    Off -> pure $ Message "Vpn is not running"
    
stop :: VpnToggle Result
stop = do
  current <- currentVpn
  case current of
    On server -> liftIO $ do
      runProcess_ $ shell $ "sudo /etc/init.d/openvpn." <> server <> " stop" 
      removeFile "/tmp/vpn/current" -- add checking status of just stopped?
      pure $ V Off
    Off -> pure $ Message "Vpn is not running"

clean :: VpnToggle Result
clean = do
  vpnInTmp <- liftIO $ fmap On (readFile "/tmp/vpn/current") `catch` exceptionHandler
  case vpnInTmp of
    On server -> liftIO $ do
      runProcess_ $ shell $ "sudo /etc/init.d/openvpn." <> server <> " stop" 
      removeFile "/tmp/vpn/current"
      pure $ V Off
    Off -> pure $ Message "No vpn in current tmp file"
  where exceptionHandler :: IOException -> IO Vpn
        exceptionHandler _ = pure Off

currentVpn :: VpnToggle Vpn
currentVpn = do
  vpnInTmp <- liftIO $ fmap On (readFile "/tmp/vpn/current") `catch` exceptionHandler
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
  (_, out, _) <- liftIO $ readProcess $ shell $ "sudo /etc/init.d/openvpn." <> server <> " status"
  case out of
    " * status: stopped\n" -> pure Off
    " * status: started\n" -> pure $ On server
    _ -> throwError "Error: something unexpected in status checking"

getLocation :: VpnToggle T.Text
getLocation = do
  response <- liftIO $ getResponseBody <$> httpJSON "http://ifconfig.co/json" :: VpnToggle Value
  case response ^? key "country" . _String of
    Just c -> pure c
    _ -> throwError "Error: something unexpected in the response from getting location"

notifyDesktop :: String -> IO ()
notifyDesktop message = do
  client <- connectSession
  void $ notify client $ blankNote {summary = "VPN",
                                    body = Just $ Text message}

nextInList :: String -> VpnToggle String
nextInList c = do
  servers <- ask
  case fmap (\i -> servers !! if i + 1 == length servers then 0 else i + 1) (c `elemIndex` servers) of
    Just server -> pure server
    Nothing -> throwError "Error: something unexpected in finding next vpn in the list"

locationString :: String -> String -> String
locationString server location = "Vpn is started. Server: " <> server <> ". Location: " <> init (tail location) <> "."
