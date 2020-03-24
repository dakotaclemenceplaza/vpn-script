#!/usr/bin/env stack
{- stack
  script
  --resolver lts-15.2
  --package "directory typed-process fdo-notify http-conduit aeson unordered-containers text mtl"
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
import Control.Concurrent (threadDelay)

data Vpn = Off | On String
type VpnToggle = ExceptT String IO Vpn

main :: IO ()
main = do
  args <- getArgs
  result <- runExceptT $ run args
  case result of
    Right vpn -> case vpn of
      On server -> putStrLn $ "Vpn is started. Location: " <> server
      Off -> putStrLn "Vpn is stopped"
    Left e -> putStrLn e

run :: [String] -> VpnToggle
run [] = start defaultServer
-- maybe getting location shouldn't be in the main monad? It shouldn't always return a vpn
-- or it needs to check if vpn is running before returning On or Off
run ["location"] = lift $ getLocation >>= \(Country c) -> notifyConnected (Country c) >> return (On (show c))
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
          threadDelay 1000000 -- needs some time before asking location
          getLocation >>= notifyConnected
          createDirectoryIfMissing False "/tmp/vpn" -- add checking status of just started vpn?
          writeFile "/tmp/vpn/current" server
          return (On server)
        On _ -> throwError "Error: running vpn is not in tmp file"

next :: VpnToggle
next = do
  current <- currentVpn
  case current of
    On c -> do void stop
               start $ nextInList c
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

nextInList :: String -> String
nextInList c = case fmap (\i -> servers !! if i + 1 == length servers then 0 else i + 1) (c `elemIndex` servers) of
  Just server -> server
  Nothing -> error "Error: something unexpected in finding next vpn in the list"


newtype Country = Country T.Text

instance FromJSON Country where
  parseJSON = withObject "Country" $ \o -> Country <$> o .: "country"
  
getLocation :: IO Country
getLocation = do
  response <- httpJSON "http://ifconfig.co/json"
  case fromJSON $ getResponseBody response of
    Success c -> return c
    _ -> error "Error: something unexpected in the response from getting location"

notifyConnected :: Country -> IO ()
notifyConnected (Country country) = do
  client <- connectSession
  let countryString = init . tail . show $ country
      message = "Location: " <> countryString
  void $ notify client $ blankNote {summary = "VPN",
                                    body = Just $ Text message}
