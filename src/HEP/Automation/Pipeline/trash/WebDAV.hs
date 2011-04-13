module HEP.Automation.Pipeline.WebDAV where

import System.Process
import System.FilePath 

data WebDAVConfig = WebDAVConfig {
        wget          :: String,
        webdav_base   :: String, 
        webdav_id     :: String, 
        webdav_passwd :: String 
        } deriving Show


fetchFile :: WebDAVConfig -> String -> String -> IO ()   
fetchFile wdavc dirname filename = do 
  readProcess (wget wdavc) 
              [ "--user=" ++ webdav_id wdavc
              , "--password=" ++ webdav_passwd wdavc 
              , webdav_base wdavc </> dirname </> filename ]
              "" 
  return ()


