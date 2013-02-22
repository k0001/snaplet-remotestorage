{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens.TH (makeLenses)
import Snap
import Network.URI as URI
import Data.Maybe
import qualified Snap.Snaplet.RemoteStorage as R
import qualified Snap.Snaplet.RemoteStorage.FileSystem as R (fsStore)

data App = App
  { _remotestorage :: Snaplet R.RemoteStorage
  }

makeLenses ''App

appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Example application" Nothing $ do
    addRoutes
      [ ("/remotestorage", with remotestorage R.root)
      , ("/webfinger", with remotestorage R.handleWebfingerSimple)
      ]
    App <$> rsS
  where
    rsS = nestSnaplet "remotestorage" remotestorage $ do
      let msr = parseURI "http://example.com/storageRoot"
          mae = parseURI "http://example.com/authEndpoint"
      R.init (fromJust msr) (fromJust mae) R.fsStore

main :: IO ()
main = serveSnaplet defaultConfig appInit
