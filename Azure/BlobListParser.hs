{-# LANGUAGE Arrows, OverloadedStrings #-}

module Azure.BlobListParser where

import Azure.BlobDataTypes

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Text.XML.HXT.Core hiding (Blob)

parse xml = runX (readString [] xml >>> getBlobs >>> xmlBlob)

getBlobs :: (ArrowXml a) => a XmlTree XmlTree
getBlobs = deep (hasName "Blob")

xmlBlob :: (ArrowXml a) => a XmlTree Blob
xmlBlob = proc tag -> do
    name <- (getText <<< getChildren <<< deep (hasName "Name")) -< tag
    url <- (getText <<< getChildren <<< deep (hasName "Url")) -< tag
    lastMod <- (getText <<< getChildren <<< deep (hasName "Last-Modified")) -< tag
    etag <- (getText <<< getChildren <<< deep (hasName "Etag")) `orElse` (constA "") -< tag
    contentLen <- (getText <<< getChildren <<< deep (hasName "Content-Length")) -< tag
    contentType <- (getText <<< getChildren <<< deep (hasName "Content-Type")) `orElse` (constA "") -< tag
    contentEnc <- (getText <<< getChildren <<< deep (hasName "Content-Encoding")) `orElse` (constA "") -< tag
    contentLang <- (getText <<< getChildren <<< deep (hasName "Content-Language")) `orElse` (constA "") -< tag
    contentMD5 <- (getText <<< getChildren <<< deep (hasName "Content-MD5")) `orElse` (constA "") -< tag
    cacheControl <- (getText <<< getChildren <<< deep (hasName "Cache-Control")) `orElse` (constA "") -< tag
    blobType <- (getText <<< getChildren <<< deep (hasName "BlobType")) -< tag
    returnA -< Blob { blobName = B8.pack name
                    , blobUrl = B8.pack url
                    , blobLastModified = B8.pack lastMod
                    , blobETag = B8.pack etag
                    , blobContentLength = read contentLen
                    , blobContentType = B8.pack contentType
                    , blobContentEncoding = B8.pack contentEnc
                    , blobContentLanguage = B8.pack contentLang
                    , blobContentMD5 = B8.pack contentMD5
                    , blobCacheControl = B8.pack cacheControl
                    , blobType = if blobType == "PageBlob" then PageBlob else BlockBlob
                    }