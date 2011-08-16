module Config (Config(..), loadConfig) where

import Language.Haskell.Interpreter

data Config = Config { ctemplates :: String
                     , cPort      :: Int
                     , cbaseUrl   :: String
                     }
            deriving Show
                     
-- The loadConfig function actually does the interesting work:                     
loadConfig :: String -> IO (Either InterpreterError Config)
loadConfig file = runInterpreter $
                  do set [languageExtensions := [ExtendedDefaultRules]]
                     loadModules [file]
                     getLoadedModules >>= setTopLevelModules
                     setImports ["Prelude"]
                     port      <- interpret "port" (as :: Int)
                     templates <- interpret "templateDir" (as :: String)
                     baseUrl   <- interpret "baseUrl" (as :: String)
                     return $ Config templates port baseUrl