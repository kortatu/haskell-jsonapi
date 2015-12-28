{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Monoid (mconcat)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

routes :: ScottyM ()
routes = do
    get  "/hello" hello
    get  "/hello/:name" helloName
    get  "/users"  listUsers
    get  "/users/:id"  showUser    
    post "/users" addUser

hello :: ActionM ()
hello = do
      text "hello world!"
      
helloName :: ActionM ()
helloName = do
      name <- param "name"
      text $ "hello " <> name <> "!"
      
listUsers :: ActionM ()
listUsers = do
      json allUsers

showUser :: ActionM ()
showUser = do
      id <- param "id"
      json (filter (matchesId id) allUsers)

addUser :: ActionM ()
addUser = do
      user <- jsonData :: ActionM User      
      json user      


main = do
  putStrLn "Starting Server..."
  scotty 3000 routes
  
data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

bob :: User
bob = User { userId = 1, userName = "bob" }

kortatu :: User
kortatu = User { userId = 4, userName = "kortatu" }

allUsers :: [User]
allUsers = [bob, kortatu]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id