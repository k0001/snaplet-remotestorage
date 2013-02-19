{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module Network.RemoteStorage.Types
  ( apiVersion
  -- * Store
  -- ** Individual items
  , ItemName
  , unItemName
  , parseItemName
  , validItemNameChar
  , ItemVersion
  , itemVersionMilliseconds
  , ItemType(..)
  -- ** Storage tree
  , ItemMap
  , Folder
  , Document
  , Node(..)
  , mkNFolder
  , nodeVersion
  , nodeItemType
  , NodePath
  , parsePath
  , isPublicPath
  , lookupFolder
  , lookupDocument
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

import qualified Data.Text as T
import qualified Data.Aeson as J
import           Data.Monoid ((<>))
import           Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Hashable as H
import qualified Data.HashMap as M
import qualified Data.Char as C
import           Data.Traversable (traverse)


--------------------------------------------------------------------------------

apiVersion :: T.Text
apiVersion = "draft-dejong-remotestorage-00"

--------------------------------------------------------------------------------

-- | An 'ItemName' is wrapper around 'Text' that can only contain only valid
-- item names.
--
-- Use the smart constructor 'parseItemName' to build an 'ItemName'.
newtype ItemName = ItemName { unItemName :: T.Text }
  deriving (Eq, Show, H.Hashable, Ord)

-- | 'Just' an 'ItemName' if the given 'T.Text' would be a valid 'ItemName',
-- otherwise 'Nothing'.
parseItemName :: T.Text -> Maybe ItemName
parseItemName ""                = Nothing
parseItemName t
    | T.all validItemNameChar t = Just $ ItemName t
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

data ItemType = Folder | Document
  deriving (Show, Eq, Ord, Enum)

instance H.Hashable ItemType where
  hashWithSalt = H.hashUsing fromEnum

-- Having 'ItemType' in the Key might seem redundant, yet we need it so that
-- we can have folders and documents with the same name.
type ItemMap a = M.Map (ItemType, ItemName) a

--------------------------------------------------------------------------------

data Folder
data Document

data Node a b x where
  NFolder   :: ItemVersion -> a -> ItemMap (Node a b x) -> Node a b Folder
  NDocument :: ItemVersion -> b                         -> Node a b Document


-- | Construct an 'Node Folder' with an optional default 'ItemVersion'. If no
-- 'ItemVersion' is given, then it is calculated from the given children.
mkNFolder :: Maybe ItemVersion -> a -> ItemMap (Node a b x) -> Node a b Folder
mkNFolder (Just ver) a xs = NFolder ver a xs
mkNFolder Nothing    a xs = NFolder ver a xs
  where ver = maximum . fmap nodeVersion $ M.elems xs


instance J.ToJSON (Node a b Folder) where
  toJSON (NFolder _ _ xs) = J.object $ M.foldWithKey pair [] xs
    where
      pair (_,ItemName n) (NFolder v _ _) = (:) $ (n <> "/") J..= showVersion v
      pair (_,ItemName n) (NDocument v _) = (:) $  n         J..= showVersion v
      showVersion = show . itemVersionMilliseconds

nodeVersion :: Node a b x -> ItemVersion
nodeVersion (NFolder v _ _) = v
nodeVersion (NDocument v _) = v

nodeItemType :: Node a b x -> ItemType
nodeItemType (NFolder _ _ _) = Folder
nodeItemType (NDocument _ _) = Document

--------------------------------------------------------------------------------

type NodePath = [ItemName]

parsePath :: T.Text -> Maybe (ItemType, NodePath)
parsePath "" = Nothing
parsePath t  = return . (,) pathType =<< path
  where path = traverse id . fmap parseItemName $ T.split (=='/') t
        isFolder = T.last t == '/'
        pathType = if isFolder then Folder else Document

isPublicPath :: NodePath -> Bool
isPublicPath (ItemName "public":_) = True
isPublicPath _                     = False

lookupFolder :: NodePath -> Node a b x -> Maybe (Node a b Folder)
lookupFolder ks x@(NFolder _ _ m) = case ks of
    []      -> Just x
    (k:ks') -> M.lookup (Folder,k) m >>= lookupFolder ks'
lookupFolder _ _ = Nothing

lookupDocument :: NodePath -> Node a b x -> Maybe (Node a b Document)
lookupDocument [] x@(NDocument _ _) = Just x
lookupDocument ks   (NFolder _ _ m) = case ks of
    []      -> Nothing
    [k]     -> M.lookup (Document,k) m >>= lookupDocument []
    (k:ks') -> M.lookup (Folder,k)   m >>= lookupDocument ks'
lookupDocument _ _ = Nothing


--------------------------------------------------------------------------------

data RequestOp
  = GetDocument
  | PutDocument
  | DelDocument
  | GetFolder
  deriving (Eq, Show, Enum)

type Request = (RequestOp, NodePath, Maybe ItemVersion)

--------------------------------------------------------------------------------

-- | A 'ModuleName' is wrapper around 'Text' that can only contain only valid
-- module names.
--
-- Use the smart constructor 'parseModuleName' to build an 'ModuleName'.
newtype ModuleName = ModuleName { unModuleName :: T.Text }
  deriving (Eq, Show)

-- | 'Just' a 'ModuleName' if the given 'T.Text' would be a valid 'ModuleName',
-- otherwise 'Nothing'.
parseModuleName :: T.Text -> Maybe ModuleName
parseModuleName ""                = Nothing
parseModuleName "public"          = Nothing
parseModuleName t
    | T.all validModuleNameChar t = Just $ ModuleName t
    | otherwise                   = Nothing


-- | Whether the given 'Char' is one of: @a-z@, @0-9@
validModuleNameChar :: Char -> Bool
validModuleNameChar c = C.isAsciiLower c || C.isDigit c

--------------------------------------------------------------------------------

data AccessLevel = Read | ReadWrite
  deriving (Eq, Show, Enum)

parseAccessLevel :: T.Text -> Maybe AccessLevel
parseAccessLevel ":r"  = Just Read
parseAccessLevel ":rw" = Just ReadWrite
parseAccessLevel _     = Nothing

--------------------------------------------------------------------------------

type AccessScope = (ModuleName, AccessLevel)

parseAccessScope :: T.Text -> Maybe AccessScope
parseAccessScope t =
    let (a,b) = T.break (==':') t in
    case (parseModuleName a, parseAccessLevel b) of
      (Just a', Just b') -> Just (a',b')
      _                  -> Nothing
