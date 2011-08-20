{-# LANGUAGE OverloadedStrings #-}
module GroceryList.Account where

import GroceryList.Datatypes
import GroceryList.Templates
import GroceryList.Forms
import GroceryList.Login
import GroceryList.Signup
import GroceryList.Tags

import Control.Applicative    
import Control.Monad.Error
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.List
import Data.Acid
import Data.Ord
import Data.IxSet hiding (null, delete)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX
import Happstack.Server       
import Text.Templating.Heist
import Text.Digestive
import Text.Digestive.Forms.Happstack
import Text.Digestive.Blaze.Html5
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Email.Validate as EValidate
import Text.Blaze (Html)
import Web.Routes.RouteT
import Web.Routes.Happstack

changePasswordFormName :: String
changePasswordFormName = "changePasswordForm"

accountFormName :: String
accountFormName = "accountForm"

data ChangePasswordForm = ChangePasswordForm String
                      
data AccountForm = AccountForm { email         :: String
                               , announcements :: Bool
                               } 
                   
accountPage :: USession -> GroceryServer Response
accountPage sess = do user <- getUserFromSess sess
                      msum [ methodM POST >> updatedAccountCheck >>= guard >> processUpdatedAccountForm sess user
                           , methodM POST >> changePasswordCheck >>= guard >> processChangePasswordForm sess user
                           , showAccountPage sess user
                           ]
              
processChangePasswordForm :: USession -> User -> GroceryServer Response
processChangePasswordForm sess user = 
  processForm (changePasswordForm (unEmail . userEmail $ user)) changePasswordFormName (showAccountPage sess user) $ 
  \(ChangePasswordForm password) -> do
    newPassword <- liftIO $ generatePassword password
    doUpdate (UpdateUser (userId user) (userEmail user) (userReceiveMail user) newPassword)
    sendFlashMessage sess "Password changed."
    seeOtherURL Account
              
processUpdatedAccountForm :: USession -> User -> GroceryServer Response
processUpdatedAccountForm sess user = 
  processForm 
  (accountForm (unEmail . userEmail $ user) (unReceive . userReceiveMail $ user))
  accountFormName 
  (showAccountPage sess user) $ 
  \(AccountForm email announcements) ->
  doUpdate (UpdateUser (userId user) (Email email) (ReceiveEmail announcements) (userPassword user)) >>
  sendFlashMessage sess "Account updated." >>
  seeOtherURL Account


changePasswordCheck :: GroceryServer Bool
changePasswordCheck = liftM (maybe False (const True)) $ optional $ look "changePassword"
                          

updatedAccountCheck :: GroceryServer Bool 
updatedAccountCheck = liftM (maybe False (const True)) $ optional $ look "updateAccount"
                   
showAccountPage :: USession -> User -> GroceryServer Response
showAccountPage sess user = do user <- getUserFromSess sess
                               changedPassword <- changePasswordCheck
                               updatedAccount  <- updatedAccountCheck
                               fmap 
                                 (bindSplices $ accountSplices user changedPassword updatedAccount)
                                 (lift (asks glsTemplates))  >>= 
                                 render' "account" >>= 
                                 ok . toResponse
                        
accountSplices :: User -> Bool -> Bool -> [(Text, Splice GroceryServer)]
accountSplices user changedPass updatedAccount = 
  [ ("changePasswordForm", formSplice' 
                           changedPass 
                           (changePasswordForm . unEmail . userEmail $ user) 
                           changePasswordFormName 
                           Account)
  , ("accountForm", formSplice' 
                    updatedAccount 
                    (accountForm (unEmail $ userEmail user) (unReceive $ userReceiveMail user)) 
                    accountFormName 
                    Account)
  ]

accountForm :: String -> Bool -> HappstackForm GroceryServer Html BlazeFormHtml AccountForm
accountForm email announcements = AccountForm
                                  <$> (((`validate` checkCurrentPassword email) $ 
                                        label "Password" ++> inputPassword) <++ errors
                                       *> validateEmailForm (Just email))
                                  <*> announcementsForm announcements

changePasswordForm :: String -> HappstackForm GroceryServer Html BlazeFormHtml ChangePasswordForm
changePasswordForm email = ChangePasswordForm 
                           <$> (((`validate` checkCurrentPassword email) $ 
                                 label "Current Password" ++> inputPassword) <++ errors
                                *> validatePasswordForm)
                           
checkCurrentPassword :: String -> Validator GroceryServer Html String
checkCurrentPassword email = checkM "Invalid password." $ checkLogin email