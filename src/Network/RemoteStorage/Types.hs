{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}


-- | This module provides types and functions to encode the remoteStorage
-- support specified by IETF's @draft-dejong-remotestorage-00.txt@ draft.


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
  , Item(..)
  -- ** Storage tree
  , Node(..)
  , mkNFolder
  , nodeVersion
  , parsePath
  , isPublicPath
  , lookupPath
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

data Item = Folder | Document
  deriving (Show, Eq, Ord, Enum)

instance H.Hashable Item where
  hashWithSalt = H.hashUsing fromEnum

--------------------------------------------------------------------------------

type NodeMap a b t = M.Map (Item, ItemName) (Node a b t)

data Node a b (x :: Item) where
  NFolder   :: ItemVersion -> a -> NodeMap a b x -> Node a b Folder
  NDocument :: ItemVersion -> b                  -> Node a b Document

type family ItemType t :: Item
type instance ItemType (Node a b Folder)   = Folder
type instance ItemType (Node a b Document) = Document

-- | Construct an 'Node Folder' with an optional default 'ItemVersion'. If no
-- 'ItemVersion' is given, then it is calculated from the given children.
mkNFolder :: Maybe ItemVersion -> a -> NodeMap a b x -> Node a b Folder
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

--------------------------------------------------------------------------------


data Path (t :: Item) where
  FolderPath   :: [ItemName] -> Path Folder
  DocumentPath :: [ItemName] -> Path Document


parsePath :: T.Text -> Maybe (Item, [ItemName])
parsePath "" = Nothing
parsePath t  = return . (,) pathType =<< path
  where path = traverse id . fmap parseItemName $ T.split (=='/') t
        isFolder = T.last t == '/'
        pathType = if isFolder then Folder else Document

isPublicPath :: Path t -> Bool
isPublicPath (FolderPath   (ItemName "public":_)) = True
isPublicPath (DocumentPath (ItemName "public":_)) = True
isPublicPath _                                    = False


lookupPath :: Path t -> Node a b t' -> Maybe (Node a b t)
lookupPath (DocumentPath _) x@(NDocument _ _) = Just x
lookupPath (DocumentPath ks)  (NFolder _ _ m) = case ks of
    []      -> Nothing
    [k]     -> M.lookup (Document,k) m >>= lookupPath (DocumentPath [])
    (k:ks') -> M.lookup (Folder,k)   m >>= lookupPath (DocumentPath ks')
lookupPath (FolderPath ks)   x@(NFolder _ _ m) = case ks of
    []      -> Just x
    (k:ks') -> M.lookup (Folder,k) m >>= lookupPath (FolderPath ks')
lookupPath _ _ = Nothing

--------------------------------------------------------------------------------

data RequestOp
  = GetDocument
  | PutDocument
  | DelDocument
  | GetFolder
  deriving (Eq, Show, Enum)

type Request (t :: Item) = (RequestOp, Path t, Maybe ItemVersion)

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
