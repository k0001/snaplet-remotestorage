{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Snap.Snaplet.RemoteStorage.FileSystem (fsStore) where

import           Codec.MIME.Parse             (parseMIMEType)
import           Codec.MIME.Type              (MIMEType, mimeType, showMIMEType)
import           Data.Aeson                   as J
import           Data.Aeson.TH                as J
import           Data.Monoid
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as BL
import qualified Network.RemoteStorage.Types  as RT
import qualified System.PosixCompat.Files     as PF
import qualified Data.Text                    as T
import           Snap

fsStore :: (MonadIO m, MonadSnap n) => RT.Store m (n ())
fsStore = RT.Store
  { RT.sGetDocument = getDocument
  , RT.sPutDocument = putDocument
  , RT.sDelDocument = delDocument
  , RT.sGetFolder   = getFolder
  }

getDocument :: (MonadIO m, MonadSnap n)
            => RT.Path -> Maybe RT.ItemVersion -> m (Either String (RT.Document, n ()))
getDocument p mv = do
  let fpath     = B.unpack $ "/tmp/rs" <> RT.bshowPath p
      fpathMeta = B.unpack $ "/tmp/rs" <> RT.bshowPath p <> ".metadata"
  ex     <- liftIO . PF.fileExist $ fpath
  exMeta <- liftIO . PF.fileExist $ fpathMeta
  if not (ex && exMeta)
    then return $ Left "Document does not exist"
    else do
      mmeta <- liftIO . readMetaFile $ fpathMeta
      case mmeta of
        Nothing -> return $ Left "Malformed document metadata."
        Just (Meta v ct) -> do
          if not (isVersion v)
            then return $ Left "Requested document version not avaiable"
            else do
              let doc = RT.Document v (unMIMEType' ct)
              return $ Right (doc, mres fpath doc)
  where
    isVersion v = maybe True (==v) mv
    mres fp (RT.Document v ct) = do
        modifyResponse $ setContentType (B.pack $ showMIMEType ct)
                       . setHeader "ETag" (B.pack $ RT.showItemVersion v)
        sendFile fp



putDocument :: MonadIO m => RT.Path -> Maybe RT.ItemVersion -> m (Either String RT.ItemVersion)
putDocument p mv = undefined

delDocument :: MonadIO m => RT.Path -> Maybe RT.ItemVersion -> m (Either String ())
delDocument p mv = undefined

getFolder :: MonadIO m => RT.Path -> Maybe RT.ItemVersion -> m (Either String RT.Folder)
getFolder p mv = undefined

--------------------------------------------------------------------------------

data Meta = Meta
  { metaVersion      :: RT.ItemVersion
  , metaContentType  :: MIMEType'
  } deriving (Eq, Show)

readMetaFile :: FilePath -> IO (Maybe Meta)
readMetaFile fp = do
  !bs <- B.readFile fp
  return . J.decode' $ BL.fromChunks [bs]

--------------------------------------------------------------------------------

-- | This is a small wrapper around 'MIMEType' that we use to provide
-- 'J.FromJSON' and 'J.ToJSON' instances.
newtype MIMEType' = MIMEType' { unMIMEType' :: MIMEType }
  deriving (Eq, Show)

instance J.FromJSON MIMEType' where
  parseJSON = J.withText "MIMEType" $ \t ->
     maybe mzero (return . MIMEType' . mimeType) . parseMIMEType $ T.unpack t

instance J.ToJSON MIMEType' where
  toJSON = J.toJSON . showMIMEType . unMIMEType'

$(J.deriveJSON (drop 4) ''Meta)
