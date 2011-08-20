{-# LANGUAGE OverloadedStrings #-}
module GroceryList.List where

import GroceryList.Datatypes
import GroceryList.Templates
import GroceryList.Forms
import GroceryList.Login
import GroceryList.Item
import GroceryList.Tags

import Control.Applicative
import Control.Monad
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
import qualified Text.Email.Validate as EValidate
import Text.Blaze (Html)
import Web.Routes.RouteT
import Web.Routes.Happstack

clearListAction :: [String] -> USession -> GroceryServer Response
clearListAction tags sess = doUpdate (ClearPurchases $ sessUser sess) >> 
                            seeOtherURL (GroceryList tags)

checkItemAction :: ItemId -> USession -> GroceryServer Response
checkItemAction iid sess =  liftIO getPOSIXTime >>= checkUncheckItemAction iid sess . Just
                                
uncheckItemAction :: ItemId -> USession -> GroceryServer Response
uncheckItemAction iid sess = checkUncheckItemAction iid sess Nothing

checkUncheckItemAction :: ItemId -> USession -> Maybe POSIXTime -> GroceryServer Response
checkUncheckItemAction iid sess time = do guard =<< ownsItem iid (sessUser sess)
                                          doUpdate $ CheckItem (sessUser sess) iid time
                                          item <- liftM (getOne . (@= iid)) $ doQuery GetItemDatabase
                                          case item of 
                                            Just i -> do
                                              links <- itemLinks (i, time)
                                              fmap (bindSplices (renderItemSplices links i))
                                                (lift (asks glsTemplates)) >>=
                                                render' "list-item" >>=
                                                ok . toResponse
                                            Nothing -> mzero
                     
listPage :: [String] -> USession -> GroceryServer Response
listPage tags sess = let sortedTags = sort tags 
                     in if sortedTags == tags 
                        then showListPage tags sess
                        else seeOtherURL $ GroceryList sortedTags

showListPage :: [String] -> USession -> GroceryServer Response
showListPage tags sess = do 
  user     <- getUserFromSess sess
  items    <- doQuery GetItemDatabase
  clearUrl <- showURL $ Clear tags
  let tagSet  = S.fromList . map Tag $ tags
      theList = M.toList .
                M.filterWithKey (\k _ -> S.isSubsetOf tagSet (itemTags k)) . 
                M.mapKeysMonotonic (idToItem items) $ 
                userList user
  fmap (bindSplices [ ("itemList",    itemListSplice itemLinks GroceryList tags theList)
                    , ("clearRoute" , textSplice $ pack clearUrl)
                    ])
    (lift (asks glsTemplates)) >>=
    render' "list" >>=
    ok . toResponse
    
itemLinks :: (Item, Maybe POSIXTime) -> GroceryServer [(Text, Splice GroceryServer)]
itemLinks (item, purchase) = do
  uncheckUrl <- showURL . Uncheck . itemId $ item
  checkUrl   <- showURL . Check .   itemId $ item
  return $ 
    [ ("itemChecked",   if isNothing purchase 
                             then return [] 
                             else runChildrenWithText 
                                  [("uncheckLink", pack uncheckUrl)])
    , ("itemUnchecked", if isJust purchase 
                             then return []
                             else runChildrenWithText 
                                  [("checkLink", pack checkUrl)])
    ]
                       
idToItem :: IxSet Item -> ItemId -> Item
idToItem items i = getOneOr (error "item key does not exist") $ items @= i