{-# LANGUAGE OverloadedStrings #-}
module GroceryList.Templates where

import Blaze.ByteString.Builder                (toLazyByteString)
import Control.Monad
import Control.Monad.Trans
import           Data.ByteString.Char8         (ByteString)
import Happstack.Server                        (Response, toResponseBS)
import Text.Templating.Heist                   

                                  
isTrueSplice :: Monad m => m Bool -> Splice m
isTrueSplice cond = do val <- lift cond
                       if val 
                         then runChildren
                         else return []
                              
isFalseSplice :: Monad m => m Bool -> Splice m
isFalseSplice = isTrueSplice . liftM not 

runWhen :: Monad m => Bool -> Splice m
runWhen x = if x then runChildren else return []

runUnless :: Monad m => Bool -> Splice m
runUnless x = if not x then runChildren else return []

-- | render using the specified template state
render':: (MonadPlus m, MonadIO m) => 
          ByteString           -- ^ template name
          -> TemplateState m  -- ^ 'TemplateDirectory' handle
          -> m Response
render' template ts = do
    t     <- renderTemplate ts template
    flip (maybe mzero) t $ \(builder, mimeType) ->
        return (toResponseBS mimeType (toLazyByteString builder))