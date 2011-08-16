{-# LANGUAGE OverloadedStrings #-}

module GroceryList.Home (homePage) where  

import GroceryList.Datatypes
import GroceryList.Templates
import GroceryList.Login

import Control.Monad.Reader
import Happstack.Server
import Web.Routes.Happstack


homePage :: GroceryServer Response
homePage = do loggedIn <- isLoggedIn
              if loggedIn
                then seeOtherURL $ GroceryList []
                else asks glsTemplates >>= render' "home" >>= ok . toResponse