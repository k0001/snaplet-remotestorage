{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Snap.Snaplet.RemoteStorage.FileSystem (fsStore) where

import Debug.Trace

import           Codec.MIME.Parse             (parseMIMEType)
import           Codec.MIME.Type              (MIMEType, mimeType, showMIMEType)
import           Data.Aeson                   as J
import           Data.Aeson.Types             as J
import           Data.Monoid
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as BL
import qualified Network.RemoteStorage.Types  as RT
import qualified System.PosixCompat.Files     as PF
import           Snap

fsStore :: (MonadIO m, MonadSnap n) => RT.Store m (n ())
fsStore = RT.Store
  { RT.sGetDocument = getDocument
  , RT.sPutDocument = putDocument
  , RT.sDelDocument = delDocument
  , RT.mGetFolder   = getFolder
  }


data Meta = Meta
  { _metaContentType  :: MIMEType
  , _metaVersion      :: RT.ItemVersion
  } deriving (Eq, Show)


instance J.ToJSON Meta where
  toJSON (Meta ct v) = J.object [ "content-type" .= showMIMEType ct
                                , "version"      .= RT.bshowItemVersion v ]

-- This parser doesn't work!
instance J.FromJSON Meta where
  parseJSON (Object v) = do
      ct  <- pMime    =<< (v .: "content-type" :: Parser String)
      ver <- pVersion =<< (v .: "version"      :: Parser Integer)
      return $ Meta ct ver
    where
      pMime :: String -> Parser MIMEType
      pMime = maybe mzero (return . mimeType) . parseMIMEType . (\a -> show a `trace` a)
      pVersion :: Integer -> Parser RT.ItemVersion
      pVersion = return . RT.itemVersionFromMilliseconds . (\a -> show a `trace` a)
  parseJSON _ = mzero

getDocument :: (MonadIO m, MonadSnap n)
            => RT.Path -> Maybe RT.ItemVersion -> m (Maybe (RT.Document, n ()))
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
        Nothing -> return Nothing
        Just (Meta ct v) -> do
          if not (versionOk v)
            then return Nothing
            else do
              let doc = RT.Document v ct
              return $ Just (doc, mres fpath doc)
  where
    readMetaFile :: FilePath -> IO (Maybe Meta)
    readMetaFile fp = do
      !bs <- B.readFile fp
      return . J.decode' $ BL.fromChunks [bs]
    versionOk v = maybe False (==v) mv
    mres fp (RT.Document v ct) = do
        modifyResponse $ setContentType (B.pack $ showMIMEType ct)
                       . setHeader "ETag" (RT.bshowItemVersion v)
        sendFile fp



putDocument :: MonadIO m => RT.Path -> Maybe RT.ItemVersion -> m (Maybe RT.ItemVersion)
putDocument p mv = undefined

delDocument :: MonadIO m => RT.Path -> Maybe RT.ItemVersion -> m Bool
delDocument p mv = undefined

getFolder :: MonadIO m => RT.Path -> Maybe RT.ItemVersion -> m (Maybe RT.Folder)
getFolder p mv = undefined



