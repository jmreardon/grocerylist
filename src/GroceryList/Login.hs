{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module GroceryList.Login where

import GroceryList.Datatypes
import GroceryList.Templates
import GroceryList.Forms
import GroceryList.Util

import Control.Applicative
import Control.Applicative.Error (maybeRead)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.Acid
import qualified Data.Foldable as F (concat)
import Data.IxSet hiding (null)
import Data.Maybe
import Data.Time.Clock.POSIX
import System.Random
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.UUID hiding (null)
import Happstack.Server       
import Text.Templating.Heist
import Text.Digestive
import Text.Digestive.Forms.Happstack
import Text.Digestive.Blaze.Html5
import Text.Blaze (Html)
import Web.Routes.RouteT
import Web.Routes.Happstack

data LoginForm = LoginForm String String

cookieLife :: POSIXTime
cookieLife = 30 * 60

sessionCookie :: String
sessionCookie = "session"

loginPage :: GroceryServer Response
loginPage =  do loggedIn <- isLoggedIn
                if loggedIn 
                  then seeOtherURL (GroceryList []) 
                  else msum [ methodM POST >> processLogin
                            , showLoginPage
                            ]
            
logoutPage :: GroceryServer Response
logoutPage = expireCookie sessionCookie >> seeOtherURL Home
            
processLogin :: GroceryServer Response
processLogin = processForm loginForm "login" showLoginPage $ 
               \(LoginForm email _) -> startSession email >> 
                                       seeOtherURL (GroceryList [])
                    
startSession :: String -> GroceryServer ()
startSession email = do
  db   <- asks glsDatabase
  sess <- asks glsSessions
  user <- liftM (getOne . (@= Email email)) $ query' db GetUserDatabase
  case user of
    Just user -> do time <- liftIO getPOSIXTime
                    uuid <- liftIO randomIO
                    _ <- update' sess $ CreateSession $ USession uuid (userId user) time []
                    updateSessionCookie uuid
    Nothing   -> return ()
    
updateSessionCookie :: UUID -> GroceryServer ()
updateSessionCookie uuid = addCookie (MaxAge . ceiling $ cookieLife) $ mkCookie sessionCookie (toString uuid)
  

showLoginPage :: GroceryServer Response
showLoginPage = asks glsTemplates >>= render' "login" >>= ok . toResponse

loginForm :: HappstackForm GroceryServer Html BlazeFormHtml LoginForm
loginForm = (`validate` validLogin) $ (<++ errors) $ LoginForm 
            <$> label "Email" ++> inputText Nothing
            <*> label "Password" ++> inputPassword
            
validLogin :: Validator GroceryServer Html LoginForm
validLogin = checkM "Email and password do not match." (\(LoginForm email pass) -> checkLogin email pass)

checkLogin :: String -> String -> GroceryServer Bool
checkLogin email password = do
  user <- findByEmail $ Email email
  return $ case user of
    Just user -> verifyPassword (encodeUtf8 . pack $ password) (userPassword user)
    Nothing   -> False
            
loginFormSplice :: Splice GroceryServer
loginFormSplice = formSplice loginForm "login" Login
                         
loggedInSplice :: Splice GroceryServer
loggedInSplice = isTrueSplice isLoggedIn
                           
loggedOutSplice :: Splice GroceryServer
loggedOutSplice = isFalseSplice isLoggedIn
                            
loginRequiredSplice :: Splice GroceryServer
loginRequiredSplice = do loginRequired <- lift . liftM (fromMaybe False . join . fmap (maybeRead . capitalize)) $ 
                                          optional $ look "login_required"
                         if loginRequired
                           then runChildren
                           else return []
                                
flashMessagesSplice ::Splice GroceryServer
flashMessagesSplice = do user     <- asks glsUser
                         sessDb   <- asks glsSessions
                         messages <- case user of 
                           Just sess -> update' sessDb $ PopFlashMessages sess
                           Nothing   -> return []
                         liftIO $ putStrLn $ "Running flash message splice" ++ show messages
                         runChildrenWith [ ("noMessages", runWhen $ null messages)
                                         , ("messages",   runUnless' 
                                                          (null messages)
                                                          [("message", mapSplices renderMessage messages)])
                                         ]
  where renderMessage (FlashMessage msg) = runChildrenWithText [("msg", pack msg)]
        
isLoggedIn :: GroceryServer Bool
isLoggedIn = fmap isJust (asks glsUser)

data SessionRefresh = Update | NoUpdate

getUser :: SessionRefresh -> GroceryServer a -> GroceryServer a
getUser s a = getUser' s (const a) a

reqUser :: SessionRefresh -> (USession -> GroceryServer Response) -> GroceryServer Response
reqUser s a = getUser' s a $
              showURL Login >>= 
              \x -> seeOther (x ++ "?login_required=true") (toResponse ()) 
                           
getUser' :: SessionRefresh -> (USession -> GroceryServer a) -> GroceryServer a -> GroceryServer a
getUser' s sessServer server = do 
  sessions <- asks glsSessions
  currTime <- liftIO getPOSIXTime
  sessId   <- liftM (>>= fromString) $ optional $ lookCookieValue sessionCookie
  case sessId of
    Just uuid -> do session <- update' sessions (UpdateSession uuid currTime cookieLife)          
                    maybe 
                      server
                      (\x -> local (\gls -> gls { glsUser = Just x }) 
                             (doUpdate uuid >> sessServer x)) 
                      session
    Nothing   -> server
  where doUpdate uuid = case s of
          Update   -> updateSessionCookie uuid
          NoUpdate -> return ()
                    
getUserFromSess :: USession -> GroceryServer User
getUserFromSess sess = liftM (getOne . (@= sessUser sess)) (doQuery GetUserDatabase) >>=
                          maybe (throwError "Session user non-existant") return