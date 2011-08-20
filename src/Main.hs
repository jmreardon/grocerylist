{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import Config
import GroceryList.Datatypes
import GroceryList.SiteMap
import GroceryList.Templates

import GroceryList.Login
import GroceryList.Signup

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Acid
import Data.IxSet hiding (null)
import qualified Data.Map as M
import Data.Text (Text)
import Happstack.Server    
import Happstack.Server.FileServe.BuildingBlocks    
import Web.Routes.Happstack    (implSite)

import System.Environment
import System.Log.Logger
import System.Posix.Signals hiding (Handler)
import System.Posix.IO ( stdInput )
import System.Posix.Terminal ( queryTerminal ) 

import Text.Templating.Heist
import Heist.Splices.Async (heistAsyncSplices)

main :: IO ()
main = do args <- getArgs
          when (null args) (fail "Must pass configuration file as parameter")
          config <- loadConfig (head args)
          case config of
            Left err -> fail ("Could not load configuration:" ++ show err)
            Right  c -> runApp c
            
runApp :: Config -> IO ()
runApp (Config templateDir httpPort baseUrl) = do
  updateGlobalLogger "" (setLevel DEBUG)
  templates    <- initGroceryTemplateState templateDir
  sess         <- initSessionDatabase
  info         <- initGroceryDatabase
  tid          <- forkIO $ simpleHTTP (nullConf { port = httpPort }) $ 
                  decodeBody (defaultBodyPolicy "/tmp/" 0 4096 4096) >>
                  msum [ mapServerPartT 
                         (unpackGLServer (GLS templates sess info Nothing)) 
                         (implSite baseUrl "" site)
                       , dir "default.appcache" $ serveFile (asContentType "text/cache-manifest") "static/default.appcache"
                       , serveDirectory DisableBrowsing [] "static" 
                       ]
  cid          <- forkIO $ checkpointState sess info
  waitForTermination
  putStrLn "Shutting Down"
  killThread tid
  killThread cid
  createCheckpointAndClose sess
  createCheckpointAndClose info

checkpointState :: AcidState SessionDatabase -> AcidState GroceryDatabase -> IO ()
checkpointState sess info = forever $ do
  threadDelay (1000000 * 60 * 60) -- yield for 5 minutes
  createCheckpoint sess
  createCheckpoint info


initTemplateState :: MonadIO m => String -> IO (TemplateState m)
initTemplateState d = do 
  templates <- loadTemplates d (emptyTemplateState d)
  case templates of
    Left err  -> fail ("Could not load templates: " ++ err)
    Right tpl -> return tpl


initGroceryTemplateState :: String -> IO (TemplateState GroceryServer)
initGroceryTemplateState d = initTemplateState d >>= return . bindSplices splices
    
splices :: [(Text, Splice GroceryServer)]
splices = [ ("loggedIn",      loggedInSplice)
          , ("loggedOut",     loggedOutSplice)
          , ("loginForm",     loginFormSplice)
          , ("loginRequired", loginRequiredSplice)
          , ("signupForm",    signupFormSplice)
          , ("flashMessages", flashMessagesSplice)
          ] ++ pathSplices ++ heistAsyncSplices


waitForTermination :: IO ()
waitForTermination = do
  istty <- queryTerminal stdInput
  mv <- newEmptyMVar
  _  <- installHandler softwareTermination (CatchOnce (putMVar mv ())) Nothing
  when istty $ installHandler keyboardSignal (CatchOnce (putMVar mv ())) Nothing >> return ()
  takeMVar mv
  
initSessionDatabase :: IO (AcidState SessionDatabase)
initSessionDatabase = openAcidState (SD empty)

initGroceryDatabase :: IO (AcidState GroceryDatabase)
initGroceryDatabase = openAcidState (GD empty empty 1)
                           
serveDirectory' :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadIO m, MonadPlus m) =>
                  FilePath    -- ^ file/directory to serve
                  -> m Response
serveDirectory' localPath =
    fileServe' serveFn mimeFn indexFn localPath
        where
          serveFn = filePathSendFile
          mimeFn  = guessContentTypeM (M.insert "appcache" "text/cache-manifest" mimeTypes) . takeWhile (/= '?')
          indexFn _ = mzero