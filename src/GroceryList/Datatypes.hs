{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeFamilies, TemplateHaskell, RankNTypes #-}
module GroceryList.Datatypes where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Acid.Core
import Data.ByteString (ByteString)
import Data.Char
import Data.Int
import Data.IxSet
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.SafeCopy
import Data.Typeable
import Data.Time.Calendar
import Data.Time.Clock.POSIX

import Data.UUID

import Web.Routes.RouteT

import Happstack.Server        
import Text.Templating.Heist  

-- For website

data SiteMap = Home
             | Signup
             | Login
             | Logout
             | GroceryList [String]
             | Items (Maybe Int) [String]
             | Check ItemId
             | Uncheck ItemId
             | Clear [String]
             | ListAdd ItemId
             | ListRemove ItemId
             deriving (Eq, Ord, Read, Show)
                      
data GLState = GLS { glsTemplates :: TemplateState GroceryServer
                   , glsSessions  :: AcidState SessionDatabase
                   , glsDatabase  :: AcidState GroceryDatabase
                   , glsUser      :: Maybe USession}
               
type GroceryServer = RouteT SiteMap (ServerPartT GroceryState)
type GroceryState  = (ErrorT String (ReaderT GLState IO))

data AddItemResult = NoUser | Created | Added

-- For database

data Purchase = Purchase { purchaseTime :: !POSIXTime } deriving (Typeable, Ord, Eq, Show)

data Item = Item { itemUser :: !UserId 
                 , itemName :: !Name 
                 , itemId   :: !ItemId 
                 , itemTags :: Set Tag
                 , itemPurchases :: ![Purchase] 
                 } deriving (Typeable, Eq, Show)
                            
data User = User { userId          :: !UserId 
                 , userEmail       :: !Email 
                 , userPassword    :: !Password 
                 , userReceiveMail :: !ReceiveEmail           
                 , userList        :: Map ItemId (Maybe POSIXTime)
                 } deriving (Typeable, Ord, Eq, Show)
                            
data USession = USession { sessId   :: !UUID 
                         , sessUser :: !UserId 
                         , sessTime :: !POSIXTime 
                         , sessMessages :: [FlashMessage]       
                         } deriving (Typeable, Ord, Eq, Show)
                                    
instance Ord Item where
  compare = comparing itemId

class ID a where
  getId :: a -> Int64

newtype UserId       = UserId       Int64                  deriving (Typeable, Ord, Eq, Show, Read, ID)
newtype ItemId       = ItemId       Int64                  deriving (Typeable, Ord, Eq, Show, Read, ID)
newtype Quantity     = Quantity     {unQuantity :: Int}    deriving (Typeable, Ord, Eq, Show, Read)
newtype Name         = Name         {unName     :: String} deriving (Typeable, Ord, Eq, Show, Read)
newtype Email        = Email        {unEmail    :: String} deriving (Typeable, Ord, Eq, Show, Read)
newtype Tag          = Tag          {unTag      :: String} deriving (Typeable, Ord, Eq, Show, Read)
newtype ReceiveEmail = ReceiveEmail {unReceive  :: Bool}   deriving (Typeable, Ord, Eq, Show, Read)
newtype FlashMessage = FlashMessage {unMsg      :: String} deriving (Typeable, Ord, Eq, Show, Read)

instance ID Int64 where
  getId = id

type Password = ByteString

instance Indexable USession where
  empty = ixSet [ ixFun (\(USession i _ _ _) -> [i]) 
                ] 

instance Indexable Item where
  empty = ixSet [ ixFun (\(Item u _ _ _ _) -> [u]) 
                , ixFun (\(Item _ _ i _ _) -> [i]) 
                , ixFun (\(Item _ n _ _ _) -> [n]) 
                , ixFun (\(Item _ n _ _ _) -> [Name . map toLower . unName $ n]) 
                , ixFun (\(Item _ _ _ t _) -> S.toList t)
                , ixFun (\(Item _ _ _ t _) -> [t])
                ] 
          
instance Indexable User where
  empty = ixSet [ ixFun (\(User i _ _ _ _) -> [i])
                , ixFun (\(User _ e _ _ _) -> [e])
                , ixFun (\(User _ _ _ r _) -> [r])
                ]
          
updateItem :: [String] -> Item -> Item
updateItem tags item = item { itemTags = foldr S.insert (itemTags item) (map Tag tags) }

userCheckItem :: ItemId -> Maybe POSIXTime -> User -> User
userCheckItem iid time user = user { userList = M.insert iid time $ userList user }

userRemoveItem :: ItemId -> User -> User
userRemoveItem iid user = user { userList = M.delete iid (userList user) }

userClearPurchases :: User -> ([(ItemId, Purchase)], User)
userClearPurchases user = let purchases = mapMaybe (\(x,y) -> (,) <$> pure x <*> (fmap Purchase y)) . 
                                          M.toList . 
                                          userList $ user
                          in (purchases, user { userList = M.filter isNothing $ userList user })
                             
updateItemPurchases :: [(ItemId, Purchase)] -> IxSet Item -> IxSet Item
updateItemPurchases updates items = foldr updateItemPurchase items updates
  where updateItemPurchase (iid, purchase) itemSet = 
          maybe 
          itemSet 
          (\item -> updateIx iid (item { itemPurchases = purchase : (itemPurchases item) }) itemSet) 
          (getOne . (@= iid) $ itemSet)
          
data GroceryDatabase = GD { gdItems   :: IxSet Item
                          , gdUsers   :: IxSet User
                          , gdCounter :: !Int64
                          }
                     deriving (Typeable, Show)
                              
newtype SessionDatabase = SD { sdSession :: IxSet USession }
                     deriving (Typeable, Show)
                              
lookupSession :: UUID -> Query SessionDatabase (Maybe USession)
lookupSession uuid = fmap (getOne . (@= uuid)) (asks sdSession)

createSession :: USession -> Update SessionDatabase ()
createSession s@(USession uuid _ _ _) = modify (SD . updateIx uuid s . sdSession)

deleteSession :: UUID -> Update SessionDatabase ()
deleteSession uuid = modify (SD . deleteIx uuid . sdSession)

updateSession :: UUID -> POSIXTime -> POSIXTime -> Update SessionDatabase (Maybe USession)
updateSession uuid t sessTimeout = do 
  sess <- runQuery $ lookupSession uuid
  case sess of
    Just s@(USession u user time m) -> 
      if t - sessTimeout < time
      then createSession (USession u user t m) >> return (Just s)
      else deleteSession uuid >> return Nothing
    Nothing -> return Nothing
    
pushFlashMessage :: USession -> FlashMessage -> Update SessionDatabase ()
pushFlashMessage (USession u user time msgs) msg = modify (SD . updateIx u (USession u user time (msg:msgs)) . sdSession)
    
popFlashMessages :: USession -> Update SessionDatabase [FlashMessage]
popFlashMessages (USession u user time msgs) = modify (SD . updateIx u (USession u user time []) . sdSession) >> return msgs
                              
$(makeAcidic ''SessionDatabase ['lookupSession, 
                                'createSession, 
                                'deleteSession, 
                                'updateSession,
                                'popFlashMessages,
                                'pushFlashMessage])

getUserDatabase :: Query GroceryDatabase (IxSet User)
getUserDatabase = asks gdUsers

getItemDatabase :: Query GroceryDatabase (IxSet Item)
getItemDatabase = asks gdItems

nextIdentifier :: Update GroceryDatabase Int64
nextIdentifier = do value <- gets gdCounter
                    modify $ \x -> x { gdCounter = value + 1 }
                    return value

addUser :: String -> Password -> Bool -> Update GroceryDatabase (Maybe User)
addUser email password getEmail = do 
  users <- runQuery getUserDatabase
  let duplicate = isJust $ (getOne . (@= Email email)) users
  if duplicate
    then return Nothing
    else do value <- nextIdentifier 
            let newUser = User (UserId value) (Email email) password (ReceiveEmail getEmail) M.empty
            modify $ \x -> x { gdUsers = insert newUser users }
            return $ Just newUser
            
addOrUpdateItem :: String -> [String] -> UserId -> Update GroceryDatabase Bool
addOrUpdateItem name tags userId = do 
  user  <- liftM (getOne . (@= userId)) $ runQuery getUserDatabase
  flip (maybe (return False)) user $ \u -> do
    match <- liftM (fmap (updateItem tags) . 
                    getOne . 
                    (@= (Name $ map toLower name)) . (@= userId)
                   ) $ 
             runQuery getItemDatabase
    toAdd <- maybe (createItem name tags userId) return match >>= updateItemDatabase
    return True 
    
createItem :: String -> [String] -> UserId -> Update GroceryDatabase Item
createItem name tags user = do
  value <- nextIdentifier
  return $ Item user (Name name) (ItemId value) (S.fromList . map Tag $ tags) []
  
updateItemDatabase :: Item -> Update GroceryDatabase Item
updateItemDatabase item = (modify $ \x -> x { gdItems = updateIx (itemId item) item $ gdItems x }) >>
                          return item
                          
checkItem :: UserId -> ItemId -> Maybe POSIXTime -> Update GroceryDatabase ()
checkItem uid iid time = do user <- liftM (fmap (userCheckItem iid time) . getOne . (@= uid)) $ gets gdUsers
                            case user of
                              Just u  -> modify $ \x -> x { gdUsers = updateIx uid u $ gdUsers x }
                              Nothing -> return ()
                              
listRemoveItem :: UserId -> ItemId -> Update GroceryDatabase ()
listRemoveItem uid iid = do user <- liftM (getOne . (@= uid)) $ gets gdUsers
                            flip (maybe (return ())) user $ \u -> do
                              let newUser = userRemoveItem iid u
                              modify $ \x -> x { gdUsers = updateIx uid newUser $ gdUsers x }
                              
clearPurchases :: UserId -> Update GroceryDatabase ()
clearPurchases uid = do user <- liftM (getOne . (@= uid)) $ gets gdUsers
                        flip (maybe (return ())) user $ \u -> do
                          let (purchases, newUser) = userClearPurchases u
                          modify $ \x -> x { gdUsers = updateIx uid newUser $ gdUsers x 
                                           , gdItems = updateItemPurchases purchases $ gdItems x 
                                           }
                              
$(makeAcidic ''GroceryDatabase ['getUserDatabase, 
                                'addUser, 
                                'getItemDatabase, 
                                'addOrUpdateItem, 
                                'checkItem, 
                                'listRemoveItem,
                                'clearPurchases])

$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''ItemId)
$(deriveSafeCopy 0 'base ''Quantity)
$(deriveSafeCopy 0 'base ''Name)
$(deriveSafeCopy 0 'base ''Email)
$(deriveSafeCopy 0 'base ''Tag)
$(deriveSafeCopy 0 'base ''UUID)
$(deriveSafeCopy 0 'base ''ReceiveEmail)
$(deriveSafeCopy 0 'base ''FlashMessage)

$(deriveSafeCopy 0 'base ''USession)
$(deriveSafeCopy 0 'base ''Item)
$(deriveSafeCopy 0 'base ''Purchase)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''GroceryDatabase)
$(deriveSafeCopy 0 'base ''SessionDatabase)

  

findByEmail :: Email -> GroceryServer (Maybe User)
findByEmail email = do db <- asks glsDatabase
                       liftM (getOne . (@= email)) $ query' db GetUserDatabase
                       
doUpdate :: (Data.Acid.Core.MethodState event ~ GroceryDatabase,
             UpdateEvent event) =>
            event -> GroceryServer (EventResult event)
doUpdate act = do db <- asks glsDatabase
                  update' db act
                  
doQuery :: (MethodState event ~ GroceryDatabase,
            QueryEvent event) =>
           event -> GroceryServer (EventResult event)
doQuery act = do db <- asks glsDatabase
                 query' db act