{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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
import Network.HTTP.Types (urlDecode, renderQuery, simpleQueryToQuery)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import System.Locale
import System.IO (openBinaryFile, IOMode(..))
import Data.List
import Data.Time
import Data.Char (isSpace)
import Data.CaseInsensitive (foldedCase)
import Data.Maybe (fromJust, isJust, listToMaybe, fromMaybe)
import Data.Ord (comparing)
import Network (withSocketsDo)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8

import Data.Monoid
import Control.Arrow (second)
import Control.Monad.IO.Class (liftIO)

import Data.Digest.Pure.SHA (hmacSha256, bytestringDigest)
import qualified Data.ByteString.Base64 as B64

default (Int)

maybeResponseError :: Response t -> Maybe (Int, t)
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
    let resource = "/" <> containerName
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
    let resource = "/" <> containerName
    rsp <- doRequest account authKey resource [("restype", "container")] "DELETE" "" []
    return $ maybeResponseError rsp

-- |List all blobs in a given container
listContainerRaw :: B.ByteString -- ^ The account name
              -> B.ByteString -- ^ Authorisation key
              -> B.ByteString -- ^ Container name
              -> IO (Either (Int, L.ByteString) L.ByteString) -- ^ Either the HTTP error code and content OR a list of Blobs
listContainerRaw account authKey containerName = do
    let resource = "/" <> containerName
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
    let resource = "/" <> containerName
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
      BlockBlobSettings name contents settings ->
        blockBlobApi name contents settings []
      PageBlobSettings name contentLength settings ->
        createPageBlob name contentLength settings

      FileBlobSettings name fp settings -> do
        h <- openBinaryFile fp ReadMode
        let doBlock i = do
              contents <- B.hGetSome h (4 * 1048576) -- 4 MB is max size
              if B.null contents then return $ Right (i - 1)
                else do
                  mrsp <- createBlockBlob name settings contents (toB64 i)
                  case mrsp of
                    Nothing -> doBlock (i + 1)
                    Just rsp -> return $ Left rsp
        result <- doBlock 1
        case result of
          Left err -> return $ Just err
          Right lastBlockId -> do
            putStrLn $ show lastBlockId ++ " blocks uploaded. Committing..."
            createBlobApi [] name (blockListBody lastBlockId) settings [("comp", "blocklist")]

  where
    toB64 = B64.encode . B8.pack . padZeroes . show
    -- http://gauravmantri.com/2013/05/18/windows-azure-blob-storage-dealing-with-the-specified-blob-or-block-content-is-invalid-error/
    padZeroes s | length s > maxSize = error "azurify: too big for this hack!"
                | otherwise = concatMap (const "0") [1 .. maxSize - length s] ++ s
      where maxSize = 5

    blockListBody :: Int -> B.ByteString
    blockListBody lastId = "<?xml version=\"1.0\" encoding=\"utf-8\"?><BlockList>"
                           <> commits lastId
                           <> "</BlockList>"
    commits :: Int -> B.ByteString
    commits 0 = ""
    commits i = commits (i - 1) <> "<Uncommitted>" <> toB64 i <> "</Uncommitted>"

    createBlockBlob name settings contents blockId =
      blockBlobApi name contents settings [
          ("comp", "block"), ("blockid", blockId)
        ]

    blockBlobApi = createBlobApi [("x-ms-blob-type", "BlockBlob")]

    createBlobApi :: [Header]
            -> B.ByteString -> B.ByteString -> CommonBlobSettings
            -> [(B.ByteString, B.ByteString)] -- ^ params
            -> IO (Maybe (Int, L.ByteString))
    createBlobApi headers name content conf params = do
        let resource = "/" <> containerName <> "/" <> name
        rsp <- doRequest account authKey resource params "PUT" content hdrs
        return $ maybeResponseError rsp
      where
        hdrs = blobHeaders conf headers

    blobHeaders conf extra = (
          map (second fromJust) $ filter (\(_,a) -> isJust a)
                    [ ("Content-Type", blobSettingsContentType conf)
                    , ("Content-Encoding", blobSettingsContentEncoding conf)
                    , ("Content-Language", blobSettingsContentLanguage conf)
                    , ("Content-MD5", blobSettingsContentMD5 conf)
                    , ("Cache-Control", blobSettingsCacheControl conf)
                    ]
            ) ++ extra

    createPageBlob :: B.ByteString -> Integer -> CommonBlobSettings -> IO (Maybe (Int, L.ByteString))
    createPageBlob name contentLength conf = createBlobApi
            [ ("x-ms-blob-type", "PageBlob")
            , ("x-ms-blob-content-length", B8.pack $ show $ contentLength)
            ] name "" conf []

-- |Delete a blob from a container
deleteBlob :: B.ByteString -- ^ The account name
           -> B.ByteString -- ^ The authorsation key
           -> B.ByteString -- ^ The container name
           -> B.ByteString -- ^ The blob name
           -> IO (Maybe (Int, L.ByteString)) -- ^ Nothing when successful, HTTP error code and content otherwise
deleteBlob account authKey containerName blobName = do
    let resource = "/" <> containerName <> "/" <> blobName
    rsp <- doRequest account authKey resource [] "DELETE" "" [] -- TODO: Add support for snapshots
    return $ maybeResponseError rsp

-- |Download a blob
getBlob :: B.ByteString -- ^ The account name
        -> B.ByteString -- ^ The authorisation key
        -> B.ByteString -- ^ The container name
        -> B.ByteString -- ^ The blob name
        -> IO (Either (Int, L.ByteString) L.ByteString) -- ^ Nothing when successful, HTTP error code and content otherwise
getBlob account authKey containerName blobName = do
    let resource = "/" <> containerName <> "/" <> blobName
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
    let resource = "/" <> containerName <> "/" <> blobName
    rsp <- doRequest account authKey resource [("comp", "lease")] "PUT" "" [("x-ms-lease-action", "break")]
    return $ maybeResponseError rsp

doRequest :: B.ByteString -> B.ByteString -> B.ByteString -> [(B.ByteString, B.ByteString)] -> B.ByteString -> B.ByteString -> [Header] -> IO (Response L.ByteString)
doRequest account authKey resource params reqType reqBody extraHeaders = do
    now <- liftIO httpTime
    let url = B8.unpack ("http://" <> account <> ".blob.core.windows.net" <> resource <> encodeParams params)
    initReq <- parseUrl url

    let headers = ("x-ms-version", "2011-08-18")
                : ("x-ms-date", now)
                : extraHeaders ++ requestHeaders initReq
    let getHeader hdr = listToMaybe $ map snd $ filter (\(a,_) -> a == hdr) headers
    let signData = defaultSignData { verb = reqType
                                   , contentLength = if reqType `elem` ["PUT", "DELETE"] || not (B.null reqBody) then B8.pack $ show $ B.length reqBody else ""
                                   , canonicalizedHeaders = canonicalizeHeaders headers
                                   , canonicalizedResource = canonicalizeResource account resource params
                                   , contentType = fromMaybe "" $ getHeader "Content-Type"
                                   , contentEncoding = fromMaybe "" $ getHeader "Content-Encoding"
                                   , contentLanguage = fromMaybe "" $ getHeader "Content-Language"
                                   , contentMD5 = fromMaybe "" $ getHeader "Content-MD5"
                                   , date = ""
                                   , ifModifiedSince = fromMaybe "" $ getHeader "If-Modified-Since"
                                   , ifMatch = fromMaybe "" $ getHeader "If-Match"
                                   , ifNoneMatch = fromMaybe "" $ getHeader "If-None-Match"
                                   , ifUnmodifiedSince = fromMaybe "" $ getHeader "If-Unmodified-Since"
                                   , range = fromMaybe "" $ getHeader "Range"
                                   }
    let signature = sign authKey signData
    let authHeader = ("Authorization", "SharedKey " <> account <> ":" <> signature)
    let request = initReq { method = reqType
                          , requestHeaders = authHeader : headers
                          , checkStatus = \_ _ _ -> Nothing -- don't throw an exception when a non-2xx error code is received
                          , requestBody = RequestBodyBS reqBody }

    withSocketsDo $ withManager $ \manager -> httpLbs request manager

encodeParams :: [(B.ByteString, B.ByteString)] -> B.ByteString
encodeParams = renderQuery True . simpleQueryToQuery

canonicalizeHeaders :: [Header] -> B.ByteString
canonicalizeHeaders headers = B.intercalate "\n" unfoldHeaders
    where headerStrs = map (\(a, b) -> strip $ foldedCase a <> ":" <> strip b) headers
          xmsHeaders = filter (\hdr ->  "x-ms" `B.isPrefixOf` hdr) headerStrs
          sortedHeaders = sort xmsHeaders
          unfoldHeaders = map (B8.pack . unwords . words . B8.unpack) sortedHeaders

canonicalizeResource :: B.ByteString -> B.ByteString -> [(B.ByteString, B.ByteString)] -> B.ByteString
canonicalizeResource accountName uriPath params = "/" <> accountName <> uriPath <> "\n" <> canonParams
    where canonParams = strip $ B.intercalate "\n" $ map (\(k,v) -> (urlDecode True) k <> ":" <> (urlDecode True v)) $ sortBy (comparing fst) params

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

defaultSignData :: SignData
defaultSignData = SignData undefined "" "" "" "" "" "" "" "" "" "" "" undefined undefined

stringToSign :: SignData -> B.ByteString
stringToSign SignData {..} =
    strip $ B.intercalate "\n" [verb, contentEncoding, contentLanguage, contentLength, contentMD5, contentType, date, ifModifiedSince, ifMatch, ifNoneMatch, ifUnmodifiedSince, range, canonicalizedHeaders, canonicalizedResource]

httpTime :: IO B.ByteString
httpTime = fmap (B8.pack . formatTime defaultTimeLocale "%a, %d %b %Y %X GMT") getCurrentTime

sign :: B.ByteString -> SignData -> B.ByteString
sign key = B64.encode . toStrict . bytestringDigest . hmacSha256 (toLazy $ B64.decodeLenient key) . LUTF8.fromString . B8.unpack . stringToSign

toLazy :: B8.ByteString -> LUTF8.ByteString
toLazy a = L.fromChunks [a]

toStrict :: LUTF8.ByteString -> B8.ByteString
toStrict = B.concat . L.toChunks
