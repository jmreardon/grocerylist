{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module GroceryList.Signup where

import GroceryList.Datatypes
import GroceryList.Templates
import GroceryList.Forms
import GroceryList.Login

import Control.Applicative    
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.Maybe
import Data.Monoid
import Data.Acid
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Happstack.Server       
import Text.Templating.Heist
import Text.Digestive
import Text.Digestive.Forms.Happstack
import Text.Digestive.Blaze.Html5
import qualified Text.Email.Validate as EValidate
import Text.Blaze (Html)
import Web.Routes.RouteT

data SignupForm = SignupForm String String Bool

passwordStrengthSetting :: Int
passwordStrengthSetting = 14

signupPage :: GroceryServer Response
signupPage = msum [ methodM POST >> processSignup
                  , showSignupPage
                  ]

processSignup :: GroceryServer Response
processSignup = processForm signupForm "signup" showSignupPage $ 
                \x ->
                createAccount x >>=
                maybe (return ())  (startSession . unEmail . userEmail) >>
                showURL Home >>=
                flip seeOther (toResponse ())

showSignupPage :: GroceryServer Response
showSignupPage = asks glsTemplates >>= render' "signup" >>= ok . toResponse

createAccount :: SignupForm -> GroceryServer (Maybe User)
createAccount (SignupForm email password getEmails) = do
  encodedPassword <- liftIO $ generatePassword password
  db              <- asks glsDatabase
  update' db $ AddUser email encodedPassword getEmails
  
generatePassword :: String -> IO Password
generatePassword password = makePassword (encodeUtf8 . pack $ password) passwordStrengthSetting

signupFormSplice :: Splice GroceryServer
signupFormSplice = formSplice signupForm "signup" Signup

signupForm :: HappstackForm GroceryServer Html BlazeFormHtml SignupForm
signupForm = SignupForm
             <$> validateEmailForm Nothing
             <*> validatePasswordForm
             <*> announcementsForm False
             
             
announcementsForm :: Bool -> HappstackForm GroceryServer Html BlazeFormHtml Bool
announcementsForm value = label "Receive announcement and update emails" ++> inputCheckBox value
             
validateEmailForm :: Maybe String -> HappstackForm GroceryServer Html BlazeFormHtml String
validateEmailForm value = ((`validateMany` [verifyEmail, checkUniqueEmail value]) $ label "Email" ++> inputEmail value) <++ errors
             
validatePasswordForm :: HappstackForm GroceryServer Html BlazeFormHtml String
validatePasswordForm = ((`transform` (Transformer $ return . Right . fst)) $ 
                        (`validate` verifyPasswords) $ 
                        (,)
                        <$> label "Password" ++> inputPassword
                        <*> label "Retype Password" ++> inputPassword
                       ) <++ errors
             
verifyPasswords :: Validator GroceryServer Html (String , String)
verifyPasswords = mconcat [ check "Password must be 6 or more characters long." $ (>= 6) . length . fst 
                          , check "Passwords do not match."                     $ uncurry (==)
                          ]
                  
verifyEmail :: Validator GroceryServer Html String
verifyEmail = check "Invalid email address." EValidate.isValid

checkUniqueEmail :: Maybe String -> Validator GroceryServer Html String
checkUniqueEmail current = checkM "Email address already in use." $ 
                           \email -> msum
                                     [ case current of 
                                          Just currEmail -> if email == currEmail then return True else mzero
                                          Nothing        -> mzero
                                     , do user <- findByEmail $ Email email
                                          return $ (isNothing user)
                                     ]