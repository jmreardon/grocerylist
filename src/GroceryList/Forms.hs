{-# LANGUAGE OverloadedStrings #-}
module GroceryList.Forms where

import GroceryList.Datatypes

import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.List.Split
import Data.Text (pack)
import Happstack.Server       
import Text.Templating.Heist
import Text.Blaze.Html5
import Text.Blaze.Renderer.XmlHtml (renderHtmlNodes)
import Text.Digestive
import Text.Digestive.Forms.Happstack
import Text.Digestive.Blaze.Html5
import Text.XmlHtml
import Web.Routes.RouteT
import Web.Routes.Happstack ()
                        
viewFormWithErrors :: Monad m => Form m i e v a -> String -> Environment m i -> m v
viewFormWithErrors f i e = do (v, r) <- runForm f i e
                              let errorList = case r of 
                                    Error x -> x
                                    Ok _    -> []
                              return $ unView v errorList
                              
processForm :: HappstackForm GroceryServer Html BlazeFormHtml a -> 
               String -> 
               GroceryServer Response -> 
               (a -> GroceryServer Response) -> 
               GroceryServer Response
processForm form identifier errorPage resultPage =
  eitherForm form identifier happstackEnvironment >>= 
  either (const errorPage) resultPage
                              
formSplice :: HappstackForm GroceryServer Html BlazeFormHtml a -> String -> SiteMap -> Splice GroceryServer
formSplice form identifier route = 
  do v <- lift $ msum [ methodM POST >> renderForm happstackEnvironment
                      , viewForm form identifier]
     url  <- lift $ showURL route
     let (renderedForm, encType) = renderFormHtml v
     runChildrenWithTemplates [ ("encType", [TextNode . pack . show $ encType])
                              , ("route", [TextNode . pack $ url])
                              , ("rForm", renderHtmlNodes renderedForm)]
  where renderForm = viewFormWithErrors form identifier
        
stringToListTransform :: Monad m => Transformer m e String [String]
stringToListTransform = Transformer $ return . Right . split (dropBlanks . dropDelims . onSublist $ ";")

wrapForm :: (Monad m, Functor m)	 
            => (w -> w)
            -> Form m i e (FormHtml w) a
            -> Form m i e (FormHtml w) a
wrapForm outerHtml form = mapView (mapFormHtml outerHtml) form
  where mapFormHtml outer (FormHtml enc html) = FormHtml enc (outer . html)