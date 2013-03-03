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
import qualified Data.ByteString.Char8       as B
import Data.Aeson as J
import qualified Data.Text as T
import Data.Monoid

data RemoteStorage = RemoteStorage
  { _rsStorageRoot  :: URI.URI
  , _rsAuthEndpoint :: URI.URI
  , _rsStore        :: Store
  }

type Store = R.Store Snap (Snap ())

makeLenses ''RemoteStorage

init :: URI.URI -> URI.URI -> Store -> SnapletInit b RemoteStorage
init sr ae s = mkSnaplet Nothing $ do
    return $ RemoteStorage sr ae s
  where
    mkSnaplet = makeSnaplet "remotestorage" "RemoteStorage snaplet"

root :: Handler b RemoteStorage ()
root = do
    mreq <- rsRequest
    liftIO . putStrLn $ "mreq: " ++ show mreq
    case mreq of
      Nothing -> writeText "Not a valid remoteStorage request."
      Just (r, p, mv) -> do
        store <- gets _rsStore
        case r of
          R.GetDocument -> do
            mres <- liftSnap $ R.sGetDocument store p mv
            case mres of
              Left msg -> writeText $ T.pack msg
              Right (_doc,ma) -> liftSnap $ ma
          R.GetFolder   -> do
            mres <- liftSnap $ R.sGetFolder store p mv
            case mres of
              Left msg -> writeText $ T.pack msg
              Right f@(R.Folder v _) -> do
                modifyResponse $ setContentType "application/json"
                               . setHeader "ETag" (B.pack $ R.showItemVersion v)
                writeLBS $ encode f
          R.PutDocument -> undefined
          R.DelDocument -> undefined
    return ()

rsRequest :: MonadSnap m => m (Maybe R.Request)
rsRequest = do
    rq <- getRequest
    let mpath = R.parsePath $ "/" <> rqPathInfo rq
        mversion = prqVersion rq
    case mpath of
      Nothing -> return Nothing
      Just p@(R.Path R.TFolder _) ->
        return $ case rqMethod rq of
          GET -> Just (R.GetFolder, p, mversion)
          _   -> Nothing
      Just p@(R.Path R.TDocument _) ->
        return $ case rqMethod rq of
          GET    -> Just (R.GetDocument, p, mversion)
          PUT    -> Just (R.PutDocument, p, mversion)
          DELETE -> Just (R.DelDocument, p, mversion)
          _      -> Nothing
  where
    prqVersion rq = R.parseItemVersion =<< case rqMethod rq of
      GET    -> getHeader "If-Modified-Since"   rq
      PUT    -> getHeader "If-Unmodified-Since" rq
      DELETE -> getHeader "If-Unmodified-Since" rq
      _      -> Nothing


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
