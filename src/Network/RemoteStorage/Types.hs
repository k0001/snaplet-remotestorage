{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides types and functions to safely encode the remoteStorage
-- support specified by IETF's @draft-dejong-remotestorage-00.txt@ draft.

module Network.RemoteStorage.Types
  ( apiVersion
  , apiAuthMethod
  , apiWebfingerLink
  -- * Store
  -- ** Individual items
  , ItemName
  , unItemName
  , parseItemName
  , validItemNameChar
  , ItemVersion
  , itemVersionMilliseconds
  , Item(..)
  -- ** Storage tree
  -- *** Building your storage
  , Storage
  , storage
  , document
  , folder
  -- *** Node manipulation
  , Node(..)
  , mkNFolder
  , nodeVersion
  , parsePath
  , lookupPath
  , isPublicPath
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

import qualified Network.URI           as URI
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Aeson            as J
import           Data.Monoid           ((<>))
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Hashable         as H
import qualified Data.HashMap          as M
import qualified Data.Char             as C
import           Data.Traversable      (traverse)


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
  deriving (Eq, Show, H.Hashable, Ord)

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

type ItemVersion = POSIXTime

itemVersionMilliseconds :: ItemVersion -> Integer
itemVersionMilliseconds = truncate . (*1000)

--------------------------------------------------------------------------------

data Item = Folder | Document
  deriving (Show, Eq, Ord, Enum)

instance H.Hashable Item where
  hashWithSalt = H.hashUsing fromEnum

type NamedItem = (Item, ItemName)

--------------------------------------------------------------------------------

type Storage a b = Node a b 'Folder

storage :: Maybe ItemVersion -> a -> NodeMap a b -> Storage a b
storage mver a = mkNFolder mver a

folder :: Maybe ItemVersion -> a -> NodeMap a b -> ANode a b
folder mver a m = ANode $ mkNFolder mver a m

document :: ItemVersion -> b -> ANode a b
document ver b = ANode $ NDocument ver b

--------------------------------------------------------------------------------

type NodeMap a b = M.Map NamedItem (ANode a b)

-- | 'Node' existential: ∀a,b. ∃t. Node a b t => ANode a b
data ANode a b where
  ANode :: Node a b t -> ANode a b
deriving instance (Show a, Show b) => Show (ANode a b)

-- | A 'Node' stores versioned values of type 'b' for a 'Document' or, together
-- with it children nodes, values of type 'a' for a 'Folder'.
data Node a b t where
  NFolder   :: ItemVersion -> a -> NodeMap a b -> Node a b Folder
  NDocument :: ItemVersion -> b                -> Node a b Document
deriving instance (Show a, Show b) => Show (Node a b t)


-- | Construct an 'NFolder' with an optional default 'ItemVersion'. If no
-- 'ItemVersion' is given, then it is calculated from the given children.
mkNFolder :: Maybe ItemVersion -> a -> NodeMap a b -> Node a b Folder
mkNFolder (Just ver) a xs = NFolder ver a xs
mkNFolder Nothing    a xs = NFolder ver a xs
  where ver = maximum . fmap anodeVersion $ M.elems xs

nodeVersion :: Node a b x -> ItemVersion
nodeVersion (NFolder v _ _) = v
nodeVersion (NDocument v _) = v

anodeVersion :: ANode a b -> ItemVersion
anodeVersion (ANode x) = nodeVersion x

instance J.ToJSON (Node a b Folder) where
  toJSON (NFolder _ _ xs) = J.object $ M.foldWithKey pair [] xs
    where
      pair (_,ItemName n) ax =
        let n' = T.pack . B.unpack $ n in
        case ax of
          ANode (NFolder _ _ _) -> (:) $ (n' <> "/") J..= ver ax
          ANode (NDocument _ _) -> (:) $  n'         J..= ver ax
      ver = show . itemVersionMilliseconds . anodeVersion


--------------------------------------------------------------------------------

data Path = Path Item [ItemName]
  deriving (Eq, Show)

parsePath :: B.ByteString -> Maybe Path
parsePath "" = Nothing
parsePath s  = return . pathType =<< path
  where path = traverse id . fmap parseItemName $ B.split '/' s
        isFolder = B.last s == '/'
        pathType | isFolder  = Path Folder
                 | otherwise = Path Document

isPublicPath :: Path -> Bool
isPublicPath (Path Folder   (ItemName "public":_))   = True
isPublicPath (Path Document (ItemName "public":_:_)) = True
isPublicPath _                                       = False

lookupPath :: Path -> ANode a b -> Maybe (ANode a b)
lookupPath (Path Document []) x@(ANode (NDocument _ _)) = Just x
lookupPath (Path Document ks)   (ANode (NFolder _ _ m)) = case ks of
    []      -> Nothing
    [k]     -> M.lookup (Document,k) m >>= lookupPath (Path Document [])
    (k:ks') -> M.lookup (Folder,  k) m >>= lookupPath (Path Document ks')
lookupPath (Path Folder ks)   x@(ANode (NFolder _ _ m)) = case ks of
    []      -> Just x
    (k:ks') -> M.lookup (Folder,  k) m >>= lookupPath (Path Folder ks')
lookupPath _ _ = Nothing

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

