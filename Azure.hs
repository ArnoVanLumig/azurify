{-# LANGUAGE OverloadedStrings, CPP #-}

{-|
    Azurify is an incomplete yet sort-of-functional library and command line client to access the Azure Blob Storage API

    The following features are implemented:

    * Creating and deleting containers

    * Listing the contents of a container

    * Downloading blobs

    * Uploading a new block blob if it's no larger than 64MB

    * Deleting a blob

    * Breaking a blob lease
-}
module Azure ( createContainer
             , deleteContainer
             , listContainerRaw
#ifndef NO_XML
             , listContainer
#endif
             , changeContainerACL
             , createBlob
             , deleteBlob
             , getBlob
             , breakLease
             , module Azure.BlobDataTypes) where

import Azure.BlobDataTypes
#ifndef NO_XML
import Azure.BlobListParser
#endif

import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import System.Locale
import Data.List
import Data.Time
import Data.Char (isSpace)
import Data.CaseInsensitive (foldedCase)
import Data.Maybe (fromJust, isJust)
import Network (withSocketsDo)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8

import Control.Arrow (second)
import Control.Monad.IO.Class (liftIO)

import Data.Digest.Pure.SHA (hmacSha256, bytestringDigest)
import qualified Data.ByteString.Base64 as B64

(+++) = B.append

maybeResponseError rsp = let status = (responseStatus rsp) in
    if statusCode status >= 300 || statusCode status < 200
               then Just (statusCode status, responseBody rsp)
               else Nothing

-- |Create a new container
createContainer :: B.ByteString -- ^ The account name
                -> B.ByteString -- ^ Authorisation key
                -> B.ByteString -- ^ Container name
                -> AccessControl -- ^ Access control of the container
                -> IO (Maybe (Int, L.ByteString)) -- ^ Nothing when creating was successful, otherwise HTTP status and content
createContainer account authKey containerName accessControl = do
    let resource = "/" +++ containerName
    rsp <- doRequest account authKey resource [("restype", "container")] "PUT" "" hdrs
    return $ maybeResponseError rsp
    where hdrs = case accessControl of
                    ContainerPublic -> [("x-ms-blob-public-access", "container")]
                    BlobPublic -> [("x-ms-blob-public-access", "blob")]
                    Private -> []

-- |Delete a container
deleteContainer :: B.ByteString -- ^ The account name
                -> B.ByteString -- ^ Authorisation key
                -> B.ByteString -- ^ Container name
                -> IO (Maybe (Int, L.ByteString)) -- ^ Nothing when creating was successful, otherwise HTTP status and content
deleteContainer account authKey containerName = do
    let resource = "/" +++ containerName
    rsp <- doRequest account authKey resource [("restype", "container")] "DELETE" "" []
    return $ maybeResponseError rsp

-- |List all blobs in a given container
listContainerRaw :: B.ByteString -- ^ The account name
              -> B.ByteString -- ^ Authorisation key
              -> B.ByteString -- ^ Container name
              -> IO (Either (Int, L.ByteString) L.ByteString) -- ^ Either the HTTP error code and content OR a list of Blobs
listContainerRaw account authKey containerName = do
    let resource = "/" +++ containerName
    rsp <- doRequest account authKey resource [("restype", "container"), ("comp", "list")] "GET" "" []
    case maybeResponseError rsp of
      Just err -> return $ Left err
      Nothing -> return $ Right $ responseBody rsp

#ifndef NO_XML
-- |List all blobs in a given container
listContainer :: B.ByteString -- ^ The account name
              -> B.ByteString -- ^ Authorisation key
              -> B.ByteString -- ^ Container name
              -> IO (Either (Int, L.ByteString) [Blob]) -- ^ Either the HTTP error code and content OR a list of Blobs
listContainer account authKey containerName = do
  res <- listContainerRaw account authKey containerName
  case res of
    Right raw -> fmap Right $ parse $ L8.unpack $ raw
    Left err -> return $ Left err
#endif

-- |Set the access control on a container
changeContainerACL :: B.ByteString -- ^ The account name
                   -> B.ByteString -- ^ The authorisation key
                   -> B.ByteString -- ^ Container name
                   -> AccessControl -- ^ Access control specifier
                   -> IO (Maybe (Int, L.ByteString)) -- ^ Nothing when successful, HTTP error code and content otherwise
changeContainerACL account authKey containerName accessControl = do
    let resource = "/" +++ containerName
    rsp <- doRequest account authKey resource [("restype", "container"), ("comp", "acl")] "PUT" "" hdrs
    return $ maybeResponseError rsp
    where hdrs = case accessControl of
                    ContainerPublic -> [("x-ms-blob-public-access", "container")]
                    BlobPublic -> [("x-ms-blob-public-access", "blob")]
                    Private -> []

-- |Upload a new blob to a container
createBlob :: B.ByteString -- ^ The account name
           -> B.ByteString -- ^ The authorisation key
           -> B.ByteString -- ^ Container name
           -> BlobSettings -- ^ The blob itself, note that Page blobs are *not supported*
           -> IO (Maybe (Int, L.ByteString)) -- ^ Nothing when successful, HTTP error code and content otherwise
createBlob account authKey containerName blobSettings =
    case blobSettings of
      BlockBlobSettings name contents common ->
        createBlockBlob name contents common
      PageBlobSettings name contentLength common ->
        createPageBlob name contentLength common
  where
    createBlockBlob :: B.ByteString -> B.ByteString -> CommonBlobSettings -> IO (Maybe (Int, L.ByteString))
    createBlockBlob name content conf = do
        let resource = "/" +++ containerName +++ "/" +++ name
        rsp <- doRequest account authKey resource [] "PUT" content hdrs
        return $ maybeResponseError rsp
        where hdrs = map (second fromJust) $ filter (\(_,a) -> isJust a)
                    [ ("Content-Type", blobSettingsContentType conf)
                    , ("Content-Encoding", blobSettingsContentEncoding conf)
                    , ("Content-Language", blobSettingsContentLanguage conf)
                    , ("Content-MD5", blobSettingsContentMD5 conf)
                    , ("Cache-Control", blobSettingsCacheControl conf)
                    , ("x-ms-blob-type", Just "BlockBlob") ]

    createPageBlob :: B.ByteString -> Integer -> CommonBlobSettings -> IO (Maybe (Int, L.ByteString))
    createPageBlob name contentLength conf = do
        let resource = "/" +++ containerName +++ "/" +++ name
        rsp <- doRequest account authKey resource [] "PUT" "" hdrs
        return $ maybeResponseError rsp
        where hdrs = map (second fromJust) $ filter (\(_,a) -> isJust a)
                    [ ("Content-Type", blobSettingsContentType conf)
                    , ("Content-Encoding", blobSettingsContentEncoding conf)
                    , ("Content-Language", blobSettingsContentLanguage conf)
                    , ("Content-MD5", blobSettingsContentMD5 conf)
                    , ("Cache-Control", blobSettingsCacheControl conf)
                    , ("x-ms-blob-type", Just "PageBlob")
                    , ("x-ms-blob-content-length", Just $ B8.pack $ show $ contentLength)
                    ]

-- |Delete a blob from a container
deleteBlob :: B.ByteString -- ^ The account name
           -> B.ByteString -- ^ The authorsation key
           -> B.ByteString -- ^ The container name
           -> B.ByteString -- ^ The blob name
           -> IO (Maybe (Int, L.ByteString)) -- ^ Nothing when successful, HTTP error code and content otherwise
deleteBlob account authKey containerName blobName = do
    let resource = "/" +++ containerName +++ "/" +++ blobName
    rsp <- doRequest account authKey resource [] "DELETE" "" [] -- TODO: Add support for snapshots
    return $ maybeResponseError rsp

-- |Download a blob
getBlob :: B.ByteString -- ^ The account name
        -> B.ByteString -- ^ The authorisation key
        -> B.ByteString -- ^ The container name
        -> B.ByteString -- ^ The blob name
        -> IO (Either (Int, L.ByteString) L.ByteString) -- ^ Nothing when successful, HTTP error code and content otherwise
getBlob account authKey containerName blobName = do
    let resource = "/" +++ containerName +++ "/" +++ blobName
    rsp <- doRequest account authKey resource [] "GET" "" []
    return $ case maybeResponseError rsp of
      Just err -> Left err
      Nothing -> Right $ responseBody rsp

-- |Break a lease of a blob
breakLease :: B.ByteString -- ^ The account name
           -> B.ByteString -- ^ The authorisation key
           -> B.ByteString -- ^ The container name
           -> B.ByteString -- ^ The blob name
           -> IO (Maybe (Int, L.ByteString)) -- ^ Nothing when successful, HTTP error code and content otherwise
breakLease account authKey containerName blobName = do
    let resource = "/" +++ containerName +++ "/" +++ blobName
    rsp <- doRequest account authKey resource [("comp", "lease")] "PUT" "" [("x-ms-lease-action", "break")]
    return $ maybeResponseError rsp

doRequest :: B.ByteString -> B.ByteString -> B.ByteString -> [(B.ByteString, B.ByteString)] -> B.ByteString -> B.ByteString -> [Header] -> IO (Response L.ByteString)
doRequest account authKey resource params reqType reqBody extraHeaders = do
    now <- liftIO httpTime
    withSocketsDo $ withManager $ \manager -> do
        initReq <- parseUrl $ B8.unpack ("http://" +++ account +++ ".blob.core.windows.net" +++ resource +++ encodeParams params)
        let headers = ("x-ms-version", "2011-08-18")
                    : ("x-ms-date", now)
                    : extraHeaders ++ requestHeaders initReq
        let signData = defaultSignData { verb = reqType
                                       , contentLength = if reqType `elem` ["PUT", "DELETE"] || not (B.null reqBody) then B8.pack $ show $ B.length reqBody else ""
                                       , canonicalizedHeaders = canonicalizeHeaders headers
                                       , canonicalizedResource = canonicalizeResource account resource params }
        let signature = sign authKey signData
        let authHeader = ("Authorization", "SharedKey " +++ account +++ ":" +++ signature)
        let request = initReq { method = reqType
                              , requestHeaders = authHeader : headers
                              , checkStatus = \_ _ _ -> Nothing -- don't throw an exception when a non-2xx error code is received
                              , requestBody = RequestBodyBS reqBody }
        httpLbs request manager

encodeParams :: [(B.ByteString, B.ByteString)] -> B.ByteString
encodeParams [] = ""
encodeParams ((k,v):ps) = "?" +++ k +++ "=" +++ v +++ encodeRest ps
    where encodeRest = B.concat . map (\(k,v) -> "&" +++ k +++ "=" +++ v)

canonicalizeHeaders :: [Header] -> B.ByteString
canonicalizeHeaders headers = B.intercalate "\n" unfoldHeaders
    where headerStrs = map (\(a, b) -> strip $ foldedCase a +++ ":" +++ strip b) headers
          xmsHeaders = filter (\hdr ->  "x-ms" `B.isPrefixOf` hdr) headerStrs
          sortedHeaders = sort xmsHeaders
          unfoldHeaders = map (B8.pack . unwords . words . B8.unpack) sortedHeaders

canonicalizeResource :: B.ByteString -> B.ByteString -> [(B.ByteString, B.ByteString)] -> B.ByteString
canonicalizeResource accountName uriPath params = "/" +++ accountName +++ uriPath +++ "\n" +++ canonParams
    where canonParams = strip $ B.intercalate "\n" $ map (\(k,v) -> k +++ ":" +++ v) $ sortBy (\(k1,v1) (k2,v2) -> compare k1 k2) params

strip :: B.ByteString -> B.ByteString
strip = f . f
   where f = B8.pack . reverse . dropWhile isSpace . B8.unpack

data SignData = SignData { verb :: B.ByteString
                         , contentEncoding :: B.ByteString
                         , contentLanguage :: B.ByteString
                         , contentLength :: B.ByteString
                         , contentMD5 :: B.ByteString
                         , contentType :: B.ByteString
                         , date :: B.ByteString
                         , ifModifiedSince :: B.ByteString
                         , ifMatch :: B.ByteString
                         , ifNoneMatch :: B.ByteString
                         , ifUnmodifiedSince :: B.ByteString
                         , range :: B.ByteString
                         , canonicalizedHeaders :: B.ByteString
                         , canonicalizedResource :: B.ByteString
                         }

defaultSignData = SignData undefined "" "" "" "" "" "" "" "" "" "" "" undefined undefined

stringToSign :: SignData -> B.ByteString
stringToSign (SignData verb ce clan clen cmd5 ct date ifMod ifMatch ifNMatch ifUnmod range canonHeaders canonResource) =
    strip $ B.intercalate "\n" [verb, ce, clan, clen, cmd5, ct, date, ifMod, ifMatch, ifNMatch, ifUnmod, range, canonHeaders, canonResource]

httpTime :: IO B.ByteString
httpTime = fmap (B8.pack . formatTime defaultTimeLocale "%a, %d %b %Y %X GMT") getCurrentTime

sign :: B.ByteString -> SignData -> B.ByteString
sign key = B64.encode . toStrict . bytestringDigest . hmacSha256 (toLazy $ B64.decodeLenient key) . LUTF8.fromString . B8.unpack . stringToSign

toLazy :: B8.ByteString -> LUTF8.ByteString
toLazy a = L.fromChunks [a]

toStrict :: LUTF8.ByteString -> B8.ByteString
toStrict = B.concat . L.toChunks
