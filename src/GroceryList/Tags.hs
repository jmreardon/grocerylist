{-# LANGUAGE OverloadedStrings #-}
module GroceryList.Tags where

import GroceryList.Datatypes
import GroceryList.Templates
import GroceryList.Forms
import GroceryList.Login

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
import qualified Text.Email.Validate as EValidate
import Text.Blaze (Html)
import Web.Routes.RouteT
import Web.Routes.Happstack

tagSplices :: ([String] -> SiteMap) -> [String] -> [String] -> [(Text, Splice GroceryServer)]
tagSplices link tags extras = 
  [ ("activeTags",       if null tags 
                         then return [] 
                         else activeTagSplice link tags)
  , ("noActiveTags",     if null tags 
                         then runChildren 
                         else return [])
  , ("additionalTags",   if null extras
                         then return [] 
                         else additionalTagSplice link tags extras)
  , ("noAdditionalTags", if null extras
                         then runChildren
                         else return [])
  ]

additionalTagSplice :: ([String] -> SiteMap) -> [String] -> [String] -> Splice GroceryServer
additionalTagSplice link currentTags tags = 
  tagsSplice [("tagLink", addTagSplice link currentTags )] tags
  
addTagSplice :: ([String] -> SiteMap) -> [String] -> String -> Splice GroceryServer
addTagSplice link currentTags newTag = do tagLink <- lift . 
                                                    showURL . 
                                                    link $ newTag:currentTags
                                          textSplice . pack $ tagLink

activeTagSplice :: ([String] -> SiteMap) -> [String] -> Splice GroceryServer
activeTagSplice link tags = tagsSplice [("tagLink", removeTagSplice link tags)] tags
  
removeTagSplice :: ([String] -> SiteMap) -> [String] -> String -> Splice GroceryServer
removeTagSplice link currentTags oldTag = do tagLink <- lift . 
                                                        showURL . 
                                                        link .
                                                        delete oldTag $ currentTags
                                             textSplice . pack $ tagLink

tagsSplice :: [(Text, String -> Splice GroceryServer)] -> 
              [String] -> 
              Splice GroceryServer
tagsSplice splices tags = runChildrenWith [("tag", mapSplices (renderTag splices) tags)]

renderTag :: [(Text, String -> Splice GroceryServer)] -> 
             String -> 
             Splice GroceryServer
renderTag splices t = runChildrenWith (("tagName", textSplice $ pack t):mappedTags)
  where mappedTags = map (\(x, f) -> (x, f t)) splices