{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Snap.Snaplet.RemoteStorage.FileSystem (fsStore) where

import           Codec.MIME.Parse             (parseMIMEType)
import           Codec.MIME.Type              (MIMEType, mimeType)
import           Data.Aeson                   as J
import           Data.Monoid
import           Data.Time.Clock.POSIX        (POSIXTime)
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as BL
import qualified Network.RemoteStorage.Types  as RT
import qualified System.PosixCompat.Files     as PF
import           Snap

fsStore :: MonadIO m => RT.Store m ()
fsStore = RT.Store
  { RT.sGetDocument = getDocument
  , RT.sPutDocument = putDocument
  , RT.sDelDocument = delDocument
  , RT.mGetFolder   = getFolder
  }

data Meta = Meta
  { metaContentType  :: MIMEType
  , metaVersion      :: POSIXTime
  } deriving (Eq, Show)


instance J.ToJSON Meta where
  toJSON (Meta ct v) = J.object [ "content-type" .= show ct
                                , "version"      .= ver ]
    where ver = show $ RT.itemVersionMilliseconds v

instance J.FromJSON Meta where
  parseJSON (Object v) = Meta <$> (pMime    =<< (v .: "content-type"))
                              <*> (pVersion =<< (v .: "version"))
    where
      pMime = maybe mzero (return . mimeType) . parseMIMEType
      pVersion = return . RT.itemVersionFromMilliseconds
  parseJSON _ = mzero

getDocument :: MonadIO m => RT.Path -> Maybe RT.ItemVersion -> m (Maybe (RT.Document, ()))
getDocument p mv = do
  let fpath = "/tmp/foo" <> show p
      fpathMeta = fpath <> ".metadata"
  ex     <- liftIO $ PF.fileExist fpath
  exMeta <- liftIO $ PF.fileExist fpathMeta
  if not (ex && exMeta)
    then return Nothing
    else do
      mmeta <- liftIO $ readMetaFile fpathMeta
      case mmeta of
        Nothing   -> return Nothing
        Just meta -> do
          let doc = RT.Document (metaVersion meta) (metaContentType meta)
              -- ma = sendFile fpath
          return $ Just (doc, ())
  where
    readMetaFile :: FilePath -> IO (Maybe Meta)
    readMetaFile fp = do
      !bs <- B.readFile fp
      return . J.decode' $ BL.fromChunks [bs]


putDocument :: MonadIO m => RT.Path -> Maybe RT.ItemVersion -> m (Maybe RT.ItemVersion)
putDocument p mv = undefined

delDocument :: MonadIO m => RT.Path -> Maybe RT.ItemVersion -> m Bool
delDocument p mv = undefined

getFolder :: MonadIO m => RT.Path -> Maybe RT.ItemVersion -> m (Maybe RT.Folder)
getFolder p mv = undefined
