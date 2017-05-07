{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty
import Web.Authenticate.OAuth
import Web.Twitter.Feed
import Network.Wai.Middleware.RequestLogger
import qualified Web.Twitter.Types as T
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Text.Lazy (pack, Text)


data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User


myoauth :: OAuth
myoauth = newOAuth
    { oauthServerName     = "api.twitter.com"
    , oauthConsumerKey    = ""
    , oauthConsumerSecret = ""
    }

mycred :: Credential
mycred = newCredential ""
                       ""

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jennnnnnny" }

allUsers :: [User]
allUsers = [bob, jenny]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

takeTwitBody :: [T.SimpleTweet] -> [String]
takeTwitBody  = map (\twit -> T.body twit)

twitsBodyToText :: [String] -> String
twitsBodyToText  = intercalate " !!!<br /> "



main = do
  putStrLn "Starting Server..."

  scotty 3001 $ do
    middleware logStdoutDev
    get "/hello/:name" $ do
      name <- param "name"
      result <- liftIO $ timeline myoauth mycred 20 False name
      case result of
        Left msg -> text "ERROR"
        Right k -> html $ pack $ twitsBodyToText $ takeTwitBody k
        --twitsBodyToText $ takeTwitBody k

    get "/users" $ do
      json allUsers

    get "/users/:id" $ do
      id <- param "id"
      json $ head $ filter (matchesId id) allUsers


    -- assignment: post user and print it out
    post "/users" $ do
      user <- jsonData :: ActionM User
      json user
