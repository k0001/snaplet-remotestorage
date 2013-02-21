{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.RemoteStorage
  ( RemoteStorage
  , rsStorageRoot
  , rsAuthEndpoint
  , init
  , root
  , handleWebfingerSimple
  , webfingerLink
  ) where

import Prelude hiding (init)
import Snap
import Control.Lens.TH (makeLenses)
import Network.URI as URI
import Network.RemoteStorage.Types as R
import Data.Aeson as J

data RemoteStorage = RemoteStorage
  { _rsStorageRoot  :: URI.URI
  , _rsAuthEndpoint :: URI.URI
  }

makeLenses ''RemoteStorage

init :: URI.URI -> URI.URI -> SnapletInit b RemoteStorage
init sr ae = mkSnaplet Nothing $ do
    return $ RemoteStorage sr ae
  where
    mkSnaplet = makeSnaplet "remotestorage" "RemoteStorage snaplet"

root :: Handler b RemoteStorage ()
root = route []

handleWebfingerSimple :: Handler b RemoteStorage ()
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

webfingerLink :: RemoteStorage -> J.Value
webfingerLink rs = R.apiWebfingerLink (_rsStorageRoot rs) (_rsAuthEndpoint rs)
