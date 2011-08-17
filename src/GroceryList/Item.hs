{-# LANGUAGE OverloadedStrings #-}
module GroceryList.Item where

import GroceryList.Datatypes
import GroceryList.Templates
import GroceryList.Forms
import GroceryList.Login
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

data AddItemForm = AddItemForm String [String]

addItemFormName :: String
addItemFormName = "addItem"
 
addItemForm :: HappstackForm GroceryServer Html BlazeFormHtml AddItemForm
addItemForm = AddItemForm 
              <$> label "Item" ++> inputText Nothing  `validate` check "An item needs a name." (not . null) <++ errors
              <*> label "Tags" ++> transform (inputText Nothing) stringToListTransform
              
addItemSplice :: Maybe Int -> [String] -> Splice GroceryServer
addItemSplice weeks tags = formSplice addItemForm addItemFormName . Items weeks . sort $ tags

processAddItem :: Maybe Int -> [String] -> USession -> GroceryServer Response
processAddItem weeks tags sess = processForm addItemForm addItemFormName (showItemsPage weeks tags sess) $
                           \(AddItemForm name additionalTags) -> 
                           addItemToDatabase name (S.toList $ S.fromList additionalTags `S.union` S.fromList tags) sess >> 
                           (seeOtherURL . Items weeks $ tags)

listAddItemAction :: ItemId -> USession -> GroceryServer Response
listAddItemAction iid sess = do guard =<< ownsItem iid (sessUser sess)
                                doUpdate $ CheckItem (sessUser sess) iid Nothing
                                itemResponse iid True

listRemoveItemAction :: ItemId -> USession -> GroceryServer Response
listRemoveItemAction iid sess = do guard =<< ownsItem iid (sessUser sess)
                                   doUpdate $ ListRemoveItem (sessUser sess) iid
                                   itemResponse iid False
                                  
itemResponse :: ItemId -> Bool -> GroceryServer Response
itemResponse iid inList = do item <- liftM (getOne . (@= iid)) $ doQuery GetItemDatabase
                             case item of 
                               Just i -> do
                                 links <- addItemLinks (i, inList)
                                 fmap (bindSplices (renderItemSplices links i))
                                   (lift (asks glsTemplates)) >>=
                                   render' "item" >>=
                                   ok . toResponse
                               Nothing -> mzero


itemsPage :: Maybe Int -> [String] -> USession -> GroceryServer Response
itemsPage weeks tags sess = let sortedTags = sort tags 
                     in if sortedTags == tags 
                        then msum [ methodM POST >> processAddItem weeks tags sess
                                  , showItemsPage weeks tags sess
                                  ]
                        else seeOtherURL $ Items weeks sortedTags
                             
showItemsPage :: Maybe Int -> [String] -> USession -> GroceryServer Response
showItemsPage weeks tags sess = do
  db    <- asks glsDatabase
  user  <- getUserFromSess db sess
  time  <- liftIO $ getPOSIXTime
  items <- liftM (onList (userList user) . filterItems time weeks tags user) $ 
           query' db GetItemDatabase
  fmap (bindSplices [ ("itemList",    itemListSplice addItemLinks (Items weeks) tags items)
                    , ("addItemForm", addItemSplice weeks tags)
                    , ("weekChoice",  weekChoicesSplice tags weeks)
                    , ("filtersOn",  runUnless $ weeks == Just 1 && null tags)
                    , ("filtersOff",   runWhen   $ weeks == Just 1 && null tags)
                    ])
    (lift (asks glsTemplates)) >>=
    render' "items" >>=
    ok . toResponse
  
onList :: Map ItemId a -> [Item] -> [(Item, Bool)]
onList listItems = map (\x -> (x, M.member (itemId x) listItems))
  
filterItems :: POSIXTime -> Maybe Int -> [String] -> User -> IxSet Item -> [Item]
filterItems time weeks tags user items = 
  filterOp .
  toDescList (Proxy :: Proxy Name) $ 
  foldr (\t s -> s @= (Tag t)) (items @= (userId user)) tags
  where filterOp = case weeks of
          Just y  -> filter (\x -> nextPurchaseTime time (itemPurchases x) 
                                   <= time + posixDayLength * 7 * fromIntegral y)
          Nothing -> id
          
addItemLinks :: (Item, Bool) -> GroceryServer [(Text, Splice GroceryServer)]
addItemLinks (item, onList) = do
  addUrl <- showURL . ListAdd .    itemId $ item
  remUrl <- showURL . ListRemove . itemId $ item
  return $ [ ("itemOnList",  if onList
                             then runChildrenWithText [("removeLink", pack remUrl)]
                             else return [])
           , ("itemOffList", if onList
                             then return []
                             else runChildrenWithText [("addLink", pack addUrl)])
           ]
  
nextPurchaseTime :: POSIXTime -> [Purchase] -> POSIXTime
nextPurchaseTime time []  = time
nextPurchaseTime _ [Purchase pTime] = pTime + posixDayLength * 7
nextPurchaseTime time purchases = purchaseTime (head purchases) + offset
  where offset  = snd . head $ diffs
        recents = take 6 . map purchaseTime $ purchases
        diffs   = sortBy (flip $ comparing id) . 
                  M.toList . 
                  M.fromListWith (+) . 
                  (`zip` repeat 1) . 
                  map (uncurry (-)) $ 
                  zip recents (tail recents)

renderItem :: GroceryServer [(Text, Splice GroceryServer)] -> Item -> Splice GroceryServer
renderItem s item = do
  splices <- lift s
  runChildrenWith $ renderItemSplices splices item
    
renderItemSplices :: [(Text, Splice GroceryServer)] -> Item -> [(Text, Splice GroceryServer)]
renderItemSplices splices item = 
  [ ("itemId",      textSplice . pack . show . getId . itemId $ item)
  , ("itemName",    textSplice . pack . unName . itemName $ item)
  , ("itemTagList", textSplice . pack . intercalate ", " . map unTag . S.toList  . itemTags $ item)
  , ("itemNoTags",  if S.null . itemTags $ item
                    then runChildren
                    else return [])
  , ("itemTags",    if S.null . itemTags $ item                    
                    then return []
                    else runChildrenWith [("tag", itemTagSplice item)])
  ] ++ splices
  
itemTagSplice :: Item -> Splice GroceryServer
itemTagSplice = tagsSplice [] . reverse . map unTag . S.toAscList . itemTags



itemListSplice :: ((Item, a) -> GroceryServer [(Text, Splice GroceryServer)]) -> 
                  ([String] -> SiteMap) -> 
                  [String] -> 
                  [(Item, a)] -> 
                  Splice GroceryServer
itemListSplice splices link tags items = 
  runChildrenWith $
  if null items
  then [ ("hasItems", return [])
       , ("noItems",  runChildren)
       ]
  else [ ("hasItems", hasItemsSplice splices link tags items)
       , ("noItems", return [])
       ]

hasItemsSplice :: ((Item, a) -> GroceryServer [(Text, Splice GroceryServer)]) -> 
                  ([String] -> SiteMap) ->
                  [String] -> 
                  [(Item, a)] -> 
                  Splice GroceryServer
hasItemsSplice splices link tags items =
  runChildrenWith ([ ("item", mapSplices (\x -> renderItem (splices x) (fst x)) . 
                              sortBy (comparing (itemName . fst)) $
                              items)]
                     ++ tagSplices link tags extraTags)
  where extraTags = reverse .
                    S.toAscList . 
                    S.difference (S.fromList $ 
                                  fmap unTag (map fst items >>= S.toList . itemTags)) $ 
                    S.fromList tags
                    
weekChoicesSplice :: [String] -> Maybe Int -> Splice GroceryServer
weekChoicesSplice tags activeWeek = do choices <- lift $ mapM generateChoice choices
                                       mapSplices renderChoice choices
  where renderChoice (z, x, y) = runChildrenWith [ ("choiceName", textSplice $ y)
                                                 , ("choiceLink", textSplice . pack $ z)
                                                 , ("choiceOn", if activeWeek == x
                                                                then runChildren
                                                                else return [])
                                                 , ("choiceOff", if activeWeek == x
                                                                 then return []
                                                                 else runChildren)
                                                 ]
        choices                = [ (Just 1, "1 Week")
                                 , (Just 2, "2 Weeks")
                                 , (Just 3, "3 Weeks")
                                 , (Just 4, "4 Weeks") 
                                 , (Nothing, "Show Everything")
                                 ] 
        generateChoice (x, y) = liftM (\z -> (z, x, y)) (showURL $ Items x tags)
                    
addItemToDatabase :: String -> [String] -> USession -> GroceryServer ()
addItemToDatabase name tags sess = do
                               db <- asks glsDatabase 
                               _  <- update' db $ AddOrUpdateItem name tags (sessUser sess)
                               return ()
                    
ownsItem :: ItemId -> UserId -> GroceryServer Bool
ownsItem iid uid = do db <- asks glsDatabase
                      liftM (isJust . getOne . (@= iid) . (@= uid)) (query' db GetItemDatabase)