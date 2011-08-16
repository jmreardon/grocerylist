{-# LANGUAGE OverloadedStrings #-}
module GroceryList.Lists where

import GroceryList.Datatypes
import GroceryList.Templates
import GroceryList.Forms
import GroceryList.Login

import Control.Applicative    
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.Maybe
import qualified Data.Map as M
import Data.Monoid
import Data.Acid
import Data.IxSet hiding (null)
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
import Web.Routes.Happstack

addListFormName :: String
addListFormName = "addList"

listsPage :: USession -> GroceryServer Response
listsPage sess = msum [ methodM POST >> processAddList sess
                      , showListsPage sess
                      ]


showListsPage :: USession -> GroceryServer Response
showListsPage sess = lift (asks glsTemplates) >>= 
                 return . bindSplice "lists" (listsSplice sess) >>=
                 render' "lists" >>= 
                 ok . toResponse
                 
processAddList :: USession -> GroceryServer Response
processAddList sess = processForm addListForm addListFormName (showListsPage sess) $ 
                      \x ->
                      createList x (sessUser sess) >>=
                      seeOtherURL . GroceryList
                      
createList :: String -> UserId -> GroceryServer ListId
createList name user = do db <- asks glsDatabase
                          update' db $ AddList name user
  

listsSplice :: USession -> Splice GroceryServer
listsSplice sess = do lists <- lift $ getLists sess
                      runChildrenWith $ if null lists
                                        then [ ("hasLists", return [])
                                             , ("noLists", runChildren)
                                             ]
                                        else [ ("hasLists", hasListsSplice lists)
                                             , ("noLists", return [])
                                             ]
                                             
hasListsSplice :: [ShoppingList] -> Splice GroceryServer
hasListsSplice lists = runChildrenWith [("listItem", mapSplices renderListInfo lists)]

renderListInfo :: ShoppingList -> Splice GroceryServer
renderListInfo (ShoppingList _ listId items name) = do
  listUrl <- lift $ showURL $ GroceryList listId
  runChildrenWithText [ ("listName",       pack . unName $ name)
                      , ("listId",         pack . show . getId $ listId)
                      , ("listItemsTotal", pack . show . M.size $ items)
                      , ("listItemsLeft",  pack . show . M.size . M.filter id $ items)
                      , ("listUrl",        pack listUrl)
                      ]

getLists :: USession -> GroceryServer [ShoppingList]
getLists sess = liftM (toDescList (Proxy :: Proxy Name) . (@= sessUser sess)) $ 
                asks glsDatabase >>= flip query' GetListDatabase
              
                
addListForm :: HappstackForm GroceryServer Html BlazeFormHtml String
addListForm = (`validate` check "Please enter a name" (not . null)) $ 
              label "Add List" ++> inputText Nothing
              
addListSplice :: Splice GroceryServer
addListSplice = formSplice addListForm addListFormName GroceryLists