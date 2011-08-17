{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module GroceryList.SiteMap (SiteMap(..), site, GroceryServer, unpackGLServer, GLState(..), pathSplices) where

import GroceryList.Home
import GroceryList.Login
import GroceryList.Signup
import GroceryList.List
import GroceryList.Item
import GroceryList.Datatypes

import Prelude                 hiding (head, (.))
import Control.Category        (Category((.)))

import Control.Monad.Error
import Control.Monad.Reader
import Data.Maybe              (listToMaybe)
import Data.Text               (Text, pack, unpack)
import Happstack.Server        
import Text.Boomerang.TH       (derivePrinterParsers)
import Text.Templating.Heist  
import Text.XmlHtml
import Web.Routes              (Site(..), setDefault)
import Web.Routes.RouteT
import Web.Routes.Boomerang  


unpackGLServer :: GLState -> UnWebT (ErrorT String (ReaderT GLState IO)) b -> UnWebT IO b
unpackGLServer s w = do eitherV <- flip runReaderT s . runErrorT $ w
                        return $ case eitherV of
                          Left err -> Just (Left $ toResponse $ "Catastrophic failure " ++ show err, 
                                            filterFun $ \r -> r{rsCode = 500})
                          Right x -> x
                      
$(derivePrinterParsers ''SiteMap)

sitemap :: Router SiteMap
sitemap = rHome
          <> lit "app" . rHome
          <> lit "app" </> (
            lit "signup"      . rSignup
            <> lit "login"       . rLogin
            <> lit "logout"      . rLogout
            <> lit "list"        . rGroceryList </> rList anyString
            <> lit "clear-list" . rClear </> rList anyString
            <> lit "items"       . rItems </>
            (lit "any" . rNothing <> lit "weeks-" . rJust . int ) </>
            rList anyString
            <> lit "ajax" </> (lit "check" . rCheck </> itemIdRouter
                               <> lit "uncheck" . rUncheck </> itemIdRouter
                               <> lit "list" </> (lit "add" .       rListAdd    </> itemIdRouter
                                                  <> lit "remove" . rListRemove </> itemIdRouter)
                              )
            )
itemIdRouter :: Router ItemId
itemIdRouter = xmaph (ItemId . fromIntegral) (Just . fromIntegral . getId) integer
          
route :: SiteMap -> GroceryServer Response
route url = case url of
  Home             -> getUser Update homePage
  Signup           -> getUser Update signupPage
  Login            -> getUser Update loginPage
  GroceryList tags -> reqUser Update (listPage tags)
  Clear tags       -> reqUser Update (clearListAction tags)
  Items weeks tags -> reqUser Update (itemsPage weeks tags)
  Check iid        -> reqUser Update (checkItemAction iid)
  Uncheck iid      -> reqUser Update (uncheckItemAction iid)
  ListAdd iid      -> reqUser Update (listAddItemAction iid)
  ListRemove iid   -> reqUser Update (listRemoveItemAction iid)
  Logout           -> getUser NoUpdate logoutPage
  
site :: Site SiteMap (ServerPartT GroceryState Response)
site = setDefault Home $ boomerangSite (runRouteT route) sitemap

spliceHome :: Splice GroceryServer
spliceHome = lift (showURL Home) >>= textSplice . pack

spliceSignup :: Splice GroceryServer
spliceSignup = lift (showURL Signup) >>= textSplice . pack

spliceLogin :: Splice GroceryServer
spliceLogin = lift (showURL Login) >>= textSplice . pack

spliceList :: Splice GroceryServer
spliceList = lift (showURL $ GroceryList []) >>= textSplice . pack

spliceItems :: Splice GroceryServer
spliceItems = lift (showURL $ Items (Just 1) []) >>= textSplice . pack

spliceLogout :: Splice GroceryServer
spliceLogout = lift (showURL Logout) >>= textSplice . pack

pathSplices :: [(Text, Splice GroceryServer)]
pathSplices = [ ("routeHome",   spliceHome)
              , ("routeSignup", spliceSignup)
              , ("routeLogin",  spliceLogin)
              , ("routeLogout", spliceLogout)
              , ("routeList",   spliceList)
              , ("routeItems",  spliceItems)
              ]