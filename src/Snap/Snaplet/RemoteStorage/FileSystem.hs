{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Snap.Snaplet.RemoteStorage.FileSystem (fsStore) where

import qualified Control.Exception            as E
import qualified System.Directory             as SD
import qualified System.FilePath              as SF
import           Codec.MIME.Parse             (parseMIMEType)
import           Codec.MIME.Type              (MIMEType, mimeType, showMIMEType)
import           Data.Aeson                   as J
import           Data.Aeson.TH                as J
import           Data.Monoid
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as BL
import qualified Network.RemoteStorage.Types  as RT
import qualified Data.Text                    as T
import           Data.List                    (isSuffixOf)
import           Snap
import           Control.Lens
import           Control.Monad.Trans.Maybe


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
  ex     <- liftIO . SD.doesFileExist $ fpath
  exMeta <- liftIO . SD.doesFileExist $ fpathMeta
  if not (ex && exMeta)
    then return $ Left "Document does not exist"
    else do
      mmeta <- liftIO . readMetaFile $ fpathMeta
      case mmeta of
        Nothing -> return $ Left "Malformed document metadata."
        Just (Meta v ct) -> do
          if not (isVersion v)
            then return $ Left "Requested document version not available"
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
getFolder p mv = do
  let fpath = B.unpack $ "/tmp/rs" <> RT.bshowPath p
  epaths <- liftIO . E.try $ SD.getDirectoryContents fpath
  case epaths of
    Left e -> return $ Left "Folder not found." where _ = e :: IOError
    Right paths -> do
      -- XXX: we only list children files currently
      let metaPaths = filter (isSuffixOf ".metadata") paths
      mmetas <- liftM sequence . liftIO $ forM metaPaths $ \fn -> runMaybeT $ do
        iname <- MaybeT . return $ RT.parseItemName (B.pack $ SF.dropExtension fn)
        meta  <- MaybeT . readMetaFile $ SF.joinPath [fpath, fn]
        return (RT.TDocument, iname, metaVersion meta)
      case mmetas of
        Nothing -> return $ Left "Folder metadata corrupted."
        Just ms ->
          let Just v = maximumOf (traverse._3) ms in
          if not (isVersion v)
            then return $ Left "Requested folder version not available"
            else return . Right $ RT.Folder v ms
  where
    isVersion v = maybe True (==v) mv

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
