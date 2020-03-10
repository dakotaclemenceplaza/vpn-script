#!/usr/bin/env stack
{- stack
  script
  --resolver lts-15.2
  --package "directory typed-process fdo-notify http-conduit aeson unordered-containers text"
-}

{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Process.Typed (runProcess, runProcess_, readProcess, shell)
import Control.Exception (IOException, catch)
import Control.Monad (void)
import Data.List (elemIndex)
import Network.HTTP.Simple (httpJSON, getResponseBody)
import Data.Aeson
import qualified Data.Text as T (Text)
import DBus.Notify 

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> start defaultVpn
    ["test"] -> getLocation >>= notifyConnected
    ["next"] -> next
    ["stop"] -> stop
    [country] -> if country `elem` vpns
                 then start country
                 else putStrLn "No such vpn"
    _ -> putStrLn "Too many arguments"

start country = do
  current <- currentVpn
  case current of
    Just c -> putStrLn $ "Vpn is already running: " ++ c
    Nothing -> do
      stat <- status country
      case stat of
        Stopped -> do
          void $ runProcess $ shell $ "sudo /etc/init.d/openvpn." ++ country ++ " start"
          getLocation >>= notifyConnected -- needs some time before asking location
          createDirectoryIfMissing False "/tmp/vpn" -- add checking status of just started vpn?
          writeFile "/tmp/vpn/current" country
        Started -> error "Error: running vpn is not in tmp file"

next = do
  current <- currentVpn
  case current of
    Just c -> do stop
                 start $ nextInList c
    Nothing -> putStrLn "Vpn is not running"

stop = do
  current <- currentVpn
  case current of
    Just country -> do
      runProcess_ $ shell $ "sudo /etc/init.d/openvpn." ++ country ++ " stop" 
      removeFile "/tmp/vpn/current" -- add checking status of just stopped?
    Nothing -> putStrLn "Vpn is not running"

currentVpn :: IO (Maybe String)
currentVpn = let exceptionHandler :: IOException -> IO (Maybe String)
                 exceptionHandler _ = return Nothing
             in do
  current <- fmap Just (readFile "/tmp/vpn/current") `catch` exceptionHandler
  case current of
    Just country -> do
      stat <- status country
      case stat of
        Started -> return $ Just country
        Stopped -> error "Error: current vpn in tmp file is not running"
    Nothing -> return Nothing
  
data Status = Stopped | Started

status :: String -> IO Status
status country = do
  (_, out, _) <- readProcess $ shell $ "sudo /etc/init.d/openvpn." ++ country ++ " status"
  case out of
    " * status: stopped\n" -> return Stopped
    " * status: started\n" -> return Started
    _ -> error "Error: something unexpected in status checking"
    
vpns = ["nl1", "nl2", "us1", "us2", "jp1", "jp2", "jp3"]
defaultVpn = "nl1"

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

notifyConnected (Country country) = do
  client <- connectSession
  let countryString = init . tail . show $ country
      message = "Location: " ++ countryString
  void $ notify client $ blankNote {summary = "VPN",
                                    body = Just $ Text message}
