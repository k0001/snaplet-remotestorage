module Network.RemoteStorage.Types
  ( ItemName
  , unItemName
  , itemName
  , validItemNameChar
  , ItemVersion
  , Node(..)
  , NodePath
  ) where

import qualified Data.Text as T
import qualified Codec.MIME.Type as Mime

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

type ItemVersion = Integer

--------------------------------------------------------------------------------

type NodePath = [ItemName]

data Node
  = Folder ItemName ItemVersion [Node]
  | Document ItemName ItemVersion Mime.MIMEType
  deriving (Eq, Show)

