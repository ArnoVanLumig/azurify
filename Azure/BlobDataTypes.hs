module Azure.BlobDataTypes where

import qualified Data.ByteString as B

data AccessControl = ContainerPublic -- ^ the container can be enumerated and all blobs can be read by anonymous users
                   | BlobPublic      -- ^ blobs can be read by anonymous users, but the container cannot be enumerated
                   | Private         -- ^ blobs can't be read by anonymous users and the container cannot be enumerated

data BlobType = PageBlob | BlockBlob deriving (Show)

data BlobSettings = BlobSettings { blobSettingsName :: B.ByteString
                                 , blobSettingsContentType :: Maybe B.ByteString
                                 , blobSettingsContentEncoding :: Maybe B.ByteString
                                 , blobSettingsContentLanguage :: Maybe B.ByteString
                                 , blobSettingsContentMD5 :: Maybe B.ByteString
                                 , blobSettingsCacheControl :: Maybe B.ByteString
                                 , blobSettingsType :: BlobType
                                 , blobSettingsContentLength :: Maybe Integer -- ^ only for page blobs, set to Nothing for block blob
                                 , blobSettingsContents :: Maybe B.ByteString -- ^ only for block blobs, set to Empty for page blob
                                 }

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