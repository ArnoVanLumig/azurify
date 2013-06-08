module Azure.BlobDataTypes where

import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Default

data AccessControl = ContainerPublic -- ^ the container can be enumerated and all blobs can be read by anonymous users
                   | BlobPublic      -- ^ blobs can be read by anonymous users, but the container cannot be enumerated
                   | Private         -- ^ blobs can't be read by anonymous users and the container cannot be enumerated

data CommonBlobSettings = BlobSettings {
                           blobSettingsContentType :: Maybe B.ByteString
                         , blobSettingsContentEncoding :: Maybe B.ByteString
                         , blobSettingsContentLanguage :: Maybe B.ByteString
                         , blobSettingsContentMD5 :: Maybe B.ByteString
                         , blobSettingsCacheControl :: Maybe B.ByteString
                         , blobSettingsMetaData :: [(Text, Text)]
                         }

instance Default CommonBlobSettings where
  def = BlobSettings Nothing Nothing Nothing Nothing Nothing []

data BlobSettings =
        BlockBlobSettings { blockBlobName :: B.ByteString
                          , blockBlobContents :: B.ByteString
                          , blockBlobSettings :: CommonBlobSettings
                          }
      | PageBlobSettings  { pageBlobName :: B.ByteString
                          , pageBlobContentLength :: Integer
                          , pageBlobSettings :: CommonBlobSettings
                          }

data BlobType = PageBlob | BlockBlob deriving (Show)
data Blob = Blob { blobName :: B.ByteString
                 , blobUrl :: B.ByteString
                 , blobLastModified :: B.ByteString
                 , blobETag :: B.ByteString
                 , blobContentLength :: Integer
                 , blobContentType :: B.ByteString
                 , blobContentEncoding :: B.ByteString
                 , blobContentLanguage :: B.ByteString
                 , blobContentMD5 :: B.ByteString
                 , blobCacheControl :: B.ByteString
                 , blobType :: BlobType
                 } deriving (Show)
