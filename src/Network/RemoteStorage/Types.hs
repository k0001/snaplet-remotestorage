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
  , mkItemName
  , validItemNameChar
  , ItemVersion
  , itemVersionSeconds
  , itemVersionMilliseconds
  -- ** Storage tree
  , Folder
  , Document
  , Node(..)
  , mkNFolder
  , nodeVersion
  , lookupNFolder
  , lookupNDocument
  , NodePath
  -- * Requests
  , RequestOp(..)
  , Request
  -- * Modules
  , ModuleName
  , unModuleName
  , mkModuleName
  , validModuleNameChar
  ) where

import qualified Data.Text as T
import qualified Data.Aeson as J
import           Data.Monoid ((<>))
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Hashable (Hashable)
import qualified Data.HashMap as M
import qualified Data.Char as C


--------------------------------------------------------------------------------

apiVersion :: T.Text
apiVersion = "draft-dejong-remotestorage-00"

--------------------------------------------------------------------------------

-- | An 'ItemName' is wrapper around 'Text' that can only contain only valid
-- item names.
--
-- Use the smart constructor 'mkItemName' to build an 'ItemName'.
newtype ItemName = ItemName { unItemName :: T.Text }
  deriving (Eq, Show, Hashable, Ord)

-- | 'Just' an 'ItemName' if the given 'T.Text' would be a valid 'ItemName',
-- otherwise 'Nothing'.
mkItemName :: T.Text -> Maybe ItemName
mkItemName t | T.all validItemNameChar t = Just $ ItemName t
             | otherwise                 = Nothing

-- | Whether the given 'Char' is one of: @a-z@, @A-Z@, @0-9@, @%@, @-@, @_@
validItemNameChar :: Char -> Bool
validItemNameChar c = C.isAsciiUpper c || C.isAsciiLower c || C.isDigit c
                   || c == '%'         || c == '-'         || c == '_'


--------------------------------------------------------------------------------

type ItemVersion = POSIXTime

itemVersionSeconds :: ItemVersion -> Integer
itemVersionSeconds = truncate

itemVersionMilliseconds :: ItemVersion -> Integer
itemVersionMilliseconds = truncate . (*1000)

--------------------------------------------------------------------------------

data Folder
data Document

type NodeMap a b x = M.Map ItemName (Node a b x)

data Node a b x where
  NFolder   :: ItemVersion -> a -> NodeMap a b x -> Node a b Folder
  NDocument :: ItemVersion -> b                  -> Node a b Document


-- | Construct an 'Node Folder' with an optional default 'ItemVersion'. If no
-- 'ItemVersion' is given, then it is calculated from the given children.
mkNFolder :: Maybe ItemVersion -> a -> NodeMap a b x -> Node a b Folder
mkNFolder (Just ver) a xs = NFolder ver a xs
mkNFolder Nothing    a xs = NFolder ver a xs
  where ver = maximum . fmap nodeVersion $ M.elems xs


instance J.ToJSON (Node a b Folder) where
  toJSON (NFolder _ _ xs) = J.object $ M.foldWithKey pair [] xs
    where
      pair (ItemName n) (NFolder v _ _) = (:) $ (n <> "/") J..= showVersion v
      pair (ItemName n) (NDocument v _) = (:) $  n         J..= showVersion v
      showVersion = show . itemVersionMilliseconds

nodeVersion :: Node a b x -> ItemVersion
nodeVersion (NFolder v _ _) = v
nodeVersion (NDocument v _) = v

--------------------------------------------------------------------------------

type NodePath = [ItemName]

lookupNFolder :: Node a b x -> NodePath -> Maybe (Node a b Folder)
lookupNFolder x@(NFolder _ _ _) []     = Just x
lookupNFolder   (NFolder _ _ m) (k:ks) = M.lookup k m >>= flip lookupNFolder ks
lookupNFolder   _                _     = Nothing

lookupNDocument :: Node a b x -> NodePath -> Maybe (Node a b Document)
lookupNDocument x@(NDocument _ _) []     = Just x
lookupNDocument   (NFolder _ _ m) (k:ks) = M.lookup k m >>= flip lookupNDocument ks
lookupNDocument   _               _      = Nothing

--------------------------------------------------------------------------------

data RequestOp
  = GetDocument
  | PutDocument
  | DelDocument
  | GetFolder
  deriving (Eq, Show)

type Request = (RequestOp, NodePath, Maybe ItemVersion)

--------------------------------------------------------------------------------

-- | A 'ModuleName' is wrapper around 'Text' that can only contain only valid
-- module names.
--
-- Use the smart constructor 'mkModuleName' to build an 'ModuleName'.
newtype ModuleName = ModuleName { unModuleName :: T.Text }
  deriving (Eq, Show)

-- | 'Just' a 'ModuleName' if the given 'T.Text' would be a valid 'ModuleName',
-- otherwise 'Nothing'.
mkModuleName :: T.Text -> Maybe ModuleName
mkModuleName "public" = Nothing
mkModuleName t | T.all validModuleNameChar t = Just $ ModuleName t
               | otherwise                   = Nothing


-- | Whether the given 'Char' is one of: @a-z@, @A-Z@, @0-9@, @%@, @-@, @_@
validModuleNameChar :: Char -> Bool
validModuleNameChar c = C.isAsciiLower c || C.isDigit c

