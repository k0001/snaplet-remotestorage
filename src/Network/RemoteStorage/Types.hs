{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module provides types and functions to safely encode the remoteStorage
-- support specified by IETF's @draft-dejong-remotestorage-00.txt@ draft.

module Network.RemoteStorage.Types
  ( apiVersion
  , apiAuthMethod
  , apiWebfingerLink
  -- * Store
  -- ** Storage backend
  , Store(..)
  -- ** Individual items
  , ItemName
  , unItemName
  , parseItemName
  , validItemNameChar
  , ItemVersion
  , parseItemVersion
  , itemVersionMilliseconds
  , itemVersionFromMilliseconds
  , bshowItemVersion
  , ItemType(..)
  , Folder(..)
  , Document(..)
  -- ** Item paths
  , Path(..)
  , parsePath
  , isPublicPath
  , bshowPath
  , bshowFolderPath
  , bshowDocumentPath
  -- * Requests
  , RequestOp(..)
  , Request
  -- * Modules
  , ModuleName
  , unModuleName
  , parseModuleName
  , validModuleNameChar
  -- * Access Levels
  , AccessLevel(..)
  , parseAccessLevel
  -- * Access Scope
  , AccessScope
  , parseAccessScope
  ) where

import           Control.Monad
import           Codec.MIME.Parse      (parseMIMEType)
import           Codec.MIME.Type       (MIMEType, showMIMEType, mimeType)
import qualified Data.Aeson.Types      as J (Parser)
import qualified Data.Aeson            as J
import qualified Data.ByteString.Char8 as B
import qualified Data.Char             as C
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Traversable      (traverse)
import qualified Network.URI           as URI


--------------------------------------------------------------------------------

apiVersion :: B.ByteString
apiVersion = "draft-dejong-remotestorage-00"

apiAuthMethod :: B.ByteString
apiAuthMethod = "http://tools.ietf.org/html/rfc6749#section-4.2"

-- | Renders a WebFinger “links” entry for the given remoteStorage root URI and
-- authentication endpoint URI.
apiWebfingerLink :: URI.URI -> URI.URI -> J.Value
apiWebfingerLink storageRoot authEndpoint = J.object
    [ "rel"        J..= ("remotestorage" :: B.ByteString)
    , "href"       J..= URI.uriToString (const "") storageRoot ""
    , "type"       J..= apiVersion
    , "properties" J..=
        [ "auth-method"   J..= apiAuthMethod
        , "auth-endpoint" J..= URI.uriToString (const "") authEndpoint ""
        ]
    ]

--------------------------------------------------------------------------------

-- | An 'ItemName' is a 'ByteString' that can only contain valid item names.
--
-- Use the smart constructor 'parseItemName' to build an 'ItemName'.
newtype ItemName = ItemName { unItemName :: B.ByteString }
  deriving (Eq, Show, Ord)

-- | 'Just' an 'ItemName' if the given 'B.ByteString' is a valid item name
-- otherwise 'Nothing'.
parseItemName :: B.ByteString -> Maybe ItemName
parseItemName ""                = Nothing
parseItemName s
    | B.all validItemNameChar s = Just $ ItemName s
    | otherwise                 = Nothing

-- | Whether the given 'Char' is one of: @a-z@, @A-Z@, @0-9@, @%@, @-@, @_@
validItemNameChar :: Char -> Bool
validItemNameChar c = C.isAsciiUpper c || C.isAsciiLower c || C.isDigit c
                   || c == '%'         || c == '-'         || c == '_'

--------------------------------------------------------------------------------

newtype ItemVersion = ItemVersion { unItemVersion :: UTCTime }
  deriving (Eq, Show, Read, Ord, J.ToJSON, J.FromJSON)

itemVersionMilliseconds :: ItemVersion -> Integer
itemVersionMilliseconds = truncate . (*1000) . utcTimeToPOSIXSeconds . unItemVersion

itemVersionFromMilliseconds :: Integer -> ItemVersion
itemVersionFromMilliseconds i = ItemVersion . posixSecondsToUTCTime $ fromInteger i / 1000

parseItemVersion :: B.ByteString -> Maybe ItemVersion
parseItemVersion s = case reads (B.unpack s) of
    ((i,""):_) -> Just $ itemVersionFromMilliseconds i
    _          -> Nothing

bshowItemVersion :: ItemVersion -> B.ByteString
bshowItemVersion = B.pack . show . itemVersionMilliseconds

--------------------------------------------------------------------------------

data ItemType = TFolder | TDocument
  deriving (Show, Eq, Ord, Enum)

--------------------------------------------------------------------------------

data Folder = Folder ItemVersion [(ItemType, ItemName, ItemVersion)]
  deriving (Eq, Show)

data Document = Document
  { docVersion     :: ItemVersion
  , docContentType :: MIMEType
  } deriving (Eq, Show)

instance J.ToJSON Folder where
  toJSON (Folder _ xs) = J.object $ map pair xs
    where
      pair (itemt, ItemName n, ver) =
        let n'   = T.pack . B.unpack $ n
            ver' = show . itemVersionMilliseconds $ ver in
        case itemt of
          TFolder   -> (n' <> "/") J..= ver'
          TDocument ->  n'         J..= ver'

--------------------------------------------------------------------------------

toJSONMIMEType :: MIMEType -> J.Value
toJSONMIMEType = J.toJSON . showMIMEType

fromJSONMIMEType :: J.Value -> J.Parser MIMEType
fromJSONMIMEType = J.withText "MIMEType" $ \t ->
     maybe mzero (return . mimeType) . parseMIMEType $ T.unpack t

--------------------------------------------------------------------------------

data Path = Path ItemType [ItemName]
  deriving (Eq)

instance Show Path where
  show = B.unpack . bshowPath

bshowPath :: Path -> B.ByteString
bshowPath (Path TFolder    xs) = bshowFolderPath xs
bshowPath (Path TDocument  xs) = bshowDocumentPath xs

bshowFolderPath :: [ItemName] -> B.ByteString
bshowFolderPath xs = "/" <> inner <> "/"
  where inner = B.intercalate "/" $ fmap unItemName xs

bshowDocumentPath :: [ItemName] -> B.ByteString
bshowDocumentPath xs = "/" <> inner
  where inner = B.intercalate "/" $ fmap unItemName xs

parsePath :: B.ByteString -> Maybe Path
parsePath "" = Nothing
parsePath s  = return . pathType =<< path
  where path = traverse id . fmap parseItemName $ B.split '/' s
        isFolder = B.last s == '/'
        pathType | isFolder  = Path TFolder
                 | otherwise = Path TDocument

isPublicPath :: Path -> Bool
isPublicPath (Path TFolder   (ItemName "public":_))   = True
isPublicPath (Path TDocument (ItemName "public":_:_)) = True
isPublicPath _                                        = False


data Store m a = Store
  { sGetDocument :: Path -> Maybe ItemVersion -> m (Maybe (Document, a))
  , sPutDocument :: Path -> Maybe ItemVersion -> m (Maybe ItemVersion)
  , sDelDocument :: Path -> Maybe ItemVersion -> m Bool
  , mGetFolder   :: Path -> Maybe ItemVersion -> m (Maybe Folder)
  }

--------------------------------------------------------------------------------

data RequestOp
  = GetDocument
  | PutDocument
  | DelDocument
  | GetFolder
  deriving (Eq, Show, Enum)

type Request = (RequestOp, Path, Maybe ItemVersion)

--------------------------------------------------------------------------------

-- | A 'ModuleName' is a 'B.ByteString' that can only contain valid module names.
--
-- Use the smart constructor 'parseModuleName' to build an 'ModuleName'.
newtype ModuleName = ModuleName { unModuleName :: B.ByteString }
  deriving (Eq, Show)

-- | 'Just' a 'ModuleName' if the given 'B.ByteString' would be a valid
-- 'ModuleName', otherwise 'Nothing'.
parseModuleName :: B.ByteString -> Maybe ModuleName
parseModuleName ""                = Nothing
parseModuleName "public"          = Nothing
parseModuleName s
    | B.all validModuleNameChar s = Just $ ModuleName s
    | otherwise                   = Nothing

-- | Whether the given 'Char' is one of: @a-z@, @0-9@
validModuleNameChar :: Char -> Bool
validModuleNameChar c = C.isAsciiLower c || C.isDigit c

--------------------------------------------------------------------------------

data AccessLevel = Read | ReadWrite
  deriving (Eq, Show, Enum)

parseAccessLevel :: B.ByteString -> Maybe AccessLevel
parseAccessLevel "r"  = Just Read
parseAccessLevel "rw" = Just ReadWrite
parseAccessLevel _     = Nothing

--------------------------------------------------------------------------------

type AccessScope = (ModuleName, AccessLevel)

parseAccessScope :: B.ByteString -> Maybe AccessScope
parseAccessScope t =
    let (a,b) = B.break (==':') t in
    case (parseModuleName a, parseAccessLevel $ B.drop 1 b) of
      (Just a', Just b') -> Just (a',b')
      _                  -> Nothing


