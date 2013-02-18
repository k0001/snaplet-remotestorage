{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.RemoteStorage.Types
  ( ItemName
  , unItemName
  , itemName
  , validItemNameChar
  , ItemVersion(..)
  , itemVersionSeconds
  , Node(..)
  , NodePath
  , nodeRep
  , Request(..)
  , NodeRequest
  ) where

import qualified Data.Text as T
import qualified Data.Aeson as J
import           Data.Monoid ((<>))
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

--------------------------------------------------------------------------------

-- | An 'ItemName' is wrapper around 'Text' that can only contain the following
-- characters: @a-z@, @A-Z@, @0-9@, @%@, @-@, @_@.
--
-- Use the smart constructor 'itemName' to build an 'ItemName'.
newtype ItemName = ItemName { unItemName :: T.Text }
  deriving (Eq, Show)

-- | Returns 'Just' an 'ItemName' if the given 'T.Text' would be an acceptable
-- 'ItemName', otherwise 'Nothing'.
itemName :: T.Text -> Maybe ItemName
itemName t | T.all validItemNameChar t = Just $ ItemName t
           | otherwise                 = Nothing

-- | Whether the given 'Char' is one of: @a-z@, @A-Z@, @0-9@, @%@, @-@, @_@
validItemNameChar :: Char -> Bool
validItemNameChar c =
    (c >= 'a'  &&  c <= 'z') ||
    (c >= 'A'  &&  c <= 'Z') ||
    (c >= '0'  &&  c <= '9') ||
     c == '%'  ||  c == '-'  ||  c == '_'


newtype ItemVersion = ItemVersion { unItemVersion :: UTCTime }
                    deriving (Eq)

instance Show ItemVersion where
  show = show . itemVersionSeconds

itemVersionSeconds :: ItemVersion -> Integer
itemVersionSeconds = floor . utcTimeToPOSIXSeconds . unItemVersion

--------------------------------------------------------------------------------

data Node
  = Folder !ItemName !ItemVersion [Node]
  | Document !ItemName !ItemVersion
  deriving (Eq, Show)

type NodePath = [ItemName]

--------------------------------------------------------------------------------

data NodeRep
  = FolderRep J.Value
  | DocumentRep ItemVersion
  deriving (Eq, Show)

nodeRep :: Node -> NodeRep
nodeRep (Document _ v)    = DocumentRep v
nodeRep (Folder   _ _ xs) = FolderRep . J.object $ map itemPair xs
  where
    itemPair (Document (ItemName n) v)   = (n <> "/") J..= show v
    itemPair (Folder   (ItemName n) v _) =  n         J..= show v

--------------------------------------------------------------------------------

data Request
  = GetDocument
  | PutDocument
  | DelDocument
  | GetFolder
  deriving (Eq, Show)

type NodeRequest = (Request, NodePath, Maybe ItemVersion)

