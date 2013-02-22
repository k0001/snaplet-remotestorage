{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.RemoteStorage
  ( RemoteStorage
  , rsStorageRoot
  , rsAuthEndpoint
  , rsStore
  , init
  , root
  , handleWebfingerSimple
  , webfingerLink
  ) where

import Prelude hiding (init)
import Snap
import Control.Lens.TH (makeLenses)
import Network.URI as URI
import qualified Network.RemoteStorage.Types as R
import Data.Aeson as J

data RemoteStorage a = RemoteStorage
  { _rsStorageRoot  :: URI.URI
  , _rsAuthEndpoint :: URI.URI
  , _rsStore        :: R.Store Snap a
  }

makeLenses ''RemoteStorage

init :: URI.URI -> URI.URI -> R.Store Snap a
     -> SnapletInit b (RemoteStorage a)
init sr ae s = mkSnaplet Nothing $ do
    return $ RemoteStorage sr ae s
  where
    mkSnaplet = makeSnaplet "remotestorage" "RemoteStorage snaplet"

root :: Handler b (RemoteStorage a) ()
root = dir "" $ do
   npath <- R.parsePath <$> getsRequest rqPathInfo
   case npath of
     Nothing -> writeText "NOTHING!"
     Just p  -> writeText "SOMETHING!"

handleWebfingerSimple :: Handler b (RemoteStorage a) ()
handleWebfingerSimple = method GET $ do
    mres <- getQueryParam "resource"
    case mres of
      Nothing  -> do
        modifyResponse $ setResponseCode 400
        writeText $ "Missing 'resource' amigo!"
      Just res -> do
        link <- gets webfingerLink
        let resp = J.object [ "subject" .= res , "links" .= [link] ]
        writeLBS $ J.encode resp
        modifyResponse $ setContentType "application/jrd+json"

webfingerLink :: RemoteStorage a -> J.Value
webfingerLink rs = R.apiWebfingerLink (_rsStorageRoot rs) (_rsAuthEndpoint rs)
