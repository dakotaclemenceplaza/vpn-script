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

data Vpn = Off | On String
type VpnToggle = ExceptT String IO Vpn

main :: IO ()
main = do
  args <- getArgs
  result <- runExceptT $ run args
  case result of
    Right vpn -> case vpn of
      On country -> putStrLn $ "Vpn is started. Location: " <> country
      Off -> putStrLn "Vpn is stopped"
    Left e -> putStrLn e

run :: [String] -> VpnToggle
run [] = start defaultVpn
run ["test"] = lift $ getLocation >>= \(Country c) -> notifyConnected (Country c) >> return (On (show c))
run ["next"] = next
run ["stop"] = stop
run [country] = if country `elem` vpns
                 then start country
                 else throwError "No such vpn"
run _ = throwError "Too many arguments"

start :: String -> VpnToggle  
start country = do
  current <- currentVpn
  case current of
    On c -> throwError $ "Vpn is already running: " <> c
    Off -> do
      stat <- status (On country)
      case stat of
        Off -> lift $ do
          void $ runProcess $ shell $ "sudo /etc/init.d/openvpn." <> country <> " start"
          getLocation >>= notifyConnected -- needs some time before asking location
          createDirectoryIfMissing False "/tmp/vpn" -- add checking status of just started vpn?
          writeFile "/tmp/vpn/current" country
          return (On country)
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
    On country -> lift $ do
      runProcess_ $ shell $ "sudo /etc/init.d/openvpn." <> country <> " stop" 
      removeFile "/tmp/vpn/current" -- add checking status of just stopped?
      return (On country)
    Off -> throwError "Vpn is not running"

currentVpn :: VpnToggle
currentVpn = let exceptionHandler :: IOException -> IO Vpn
                 exceptionHandler _ = return Off
             in do
  current <- lift (fmap On (readFile "/tmp/vpn/current") `catch` exceptionHandler) >>= status
  case current of
    On country -> return $ On country
    Off -> throwError "Error: current vpn in tmp file is not running"

status :: Vpn -> VpnToggle
status (On country) = do
  (_, out, _) <- lift $ readProcess $ shell $ "sudo /etc/init.d/openvpn." <> country <> " status"
  case out of
    " * status: stopped\n" -> return Off
    " * status: started\n" -> return $ On country
    _ -> throwError "Error: something unexpected in status checking"
status Off = return Off

vpns :: [String]
vpns = ["nl1", "nl2", "us1", "us2", "jp1", "jp2", "jp3"]
defaultVpn :: String
defaultVpn = "nl1"

nextInList :: String -> String
nextInList c = case fmap (\i -> vpns !! if i + 1 == length vpns then 0 else i + 1) (c `elemIndex` vpns) of
  Just country -> country
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
