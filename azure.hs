{-# LANGUAGE OverloadedStrings #-}

module Azure ( createContainer
             , deleteContainer
             , listContainer
             , changeContainerACL
             , createBlob
             , deleteBlob
             , getBlob
             , breakLease
             , module BlobDataTypes) where

import BlobDataTypes
import BlobListParser

import Network.HTTP.Conduit
import Network.HTTP.Date
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import System.Locale
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import Data.Char (isSpace)
import Data.CaseInsensitive (foldedCase)
import Data.Maybe (fromJust, isJust)
import Network (withSocketsDo)

import Data.Conduit

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

createContainer :: B.ByteString -> B.ByteString -> B.ByteString -> AccessControl -> IO (Maybe (Int, L.ByteString))
createContainer account authKey containerName accessControl = do
    let resource = "/" +++ containerName
    Response status _ _ body <- doRequest account authKey resource [("restype", "container")] "PUT" "" hdrs
    return $ if statusCode status >= 300 || statusCode status < 200 then Just (statusCode status, body) else Nothing
    where hdrs = case accessControl of
                    ContainerPublic -> [("x-ms-blob-public-access", "container")]
                    BlobPublic -> [("x-ms-blob-public-access", "blob")]
                    Private -> []

deleteContainer :: B.ByteString -> B.ByteString -> B.ByteString -> IO (Maybe (Int, L.ByteString))
deleteContainer account authKey containerName = do
    let resource = "/" +++ containerName
    Response status _ _ body <- doRequest account authKey resource [("restype", "container")] "DELETE" "" []
    return $ if statusCode status >= 300 || statusCode status < 200 then Just (statusCode status, body) else Nothing

listContainer :: B.ByteString -> B.ByteString -> B.ByteString -> IO (Either (Int, L.ByteString) [Blob])
listContainer account authKey containerName = do
    let resource = "/" +++ containerName
    Response status _ _ body <- doRequest account authKey resource [("restype", "container"), ("comp", "list")] "GET" "" []
    if statusCode status >= 300 || statusCode status < 200 then
        return $ Left (statusCode status, body) else do
            blobs <- parse $ L8.unpack body
            return $ Right blobs

changeContainerACL :: B.ByteString -> B.ByteString -> B.ByteString -> AccessControl -> IO (Maybe (Int, L.ByteString))
changeContainerACL account authKey containerName accessControl = do
    let resource = "/" +++ containerName
    Response status _ _ body <- doRequest account authKey resource [("restype", "container"), ("comp", "acl")] "PUT" "" hdrs
    return $ if statusCode status >= 300 || statusCode status < 200 then Just (statusCode status, body) else Nothing
    where hdrs = case accessControl of
                    ContainerPublic -> [("x-ms-blob-public-access", "container")]
                    BlobPublic -> [("x-ms-blob-public-access", "blob")]
                    Private -> []

createBlob :: B.ByteString -> B.ByteString -> B.ByteString -> BlobSettings -> IO (Maybe (Int, L.ByteString))
createBlob account authKey containerName blobSettings =
    case blobSettingsType blobSettings of
        BlockBlob -> createBlockBlob account authKey containerName blobSettings
        PageBlob -> error "Page blob not implemented yet"

createBlockBlob :: B.ByteString -> B.ByteString -> B.ByteString -> BlobSettings -> IO (Maybe (Int, L.ByteString))
createBlockBlob account authKey containerName blobSettings = do
    let resource = "/" +++ containerName +++ "/" +++ blobSettingsName blobSettings
    Response status _ _ body <- doRequest account authKey resource [] "PUT" (fromJust $ blobSettingsContents blobSettings) hdrs
    return $ if statusCode status >= 300 || statusCode status < 200 then Just (statusCode status, body) else Nothing
    where hdrs = map (second fromJust) $ filter (\(_,a) -> isJust a) 
                [ ("Content-Type", blobSettingsContentType blobSettings)
                , ("Content-Encoding", blobSettingsContentEncoding blobSettings)
                , ("Content-Language", blobSettingsContentLanguage blobSettings)
                , ("Content-MD5", blobSettingsContentMD5 blobSettings)
                , ("Cache-Control", blobSettingsCacheControl blobSettings)
                , ("x-ms-blob-type", Just "BlockBlob") ]

createPageBlob :: B.ByteString -> B.ByteString -> B.ByteString -> BlobSettings -> IO (Maybe (Int, L.ByteString))
createPageBlob account authKey containerName blobSettings = do
    let resource = "/" +++ containerName +++ "/" +++ blobSettingsName blobSettings
    Response status _ _ body <- doRequest account authKey resource [] "PUT" "" hdrs
    return $ if statusCode status >= 300 || statusCode status < 200 then Just (statusCode status, body) else Nothing
    where hdrs = map (second fromJust) $ filter (\(_,a) -> isJust a) 
                [ ("Content-Type", blobSettingsContentType blobSettings)
                , ("Content-Encoding", blobSettingsContentEncoding blobSettings)
                , ("Content-Language", blobSettingsContentLanguage blobSettings)
                , ("Content-MD5", blobSettingsContentMD5 blobSettings)
                , ("Cache-Control", blobSettingsCacheControl blobSettings)
                , ("x-ms-blob-type", Just "PageBlob")
                , ("x-ms-blob-content-length", Just $ B8.pack $ show $ B.length $ fromJust $ blobSettingsContents blobSettings)
                ]

deleteBlob :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Maybe (Int, L.ByteString))
deleteBlob account authKey containerName blobName = do
    let resource = "/" +++ containerName +++ "/" +++ blobName
    Response status _ _ body <- doRequest account authKey resource [] "DELETE" "" [] -- TODO: Add support for snapshots
    return $ if statusCode status >= 300 || statusCode status < 200 then Just (statusCode status, body) else Nothing

getBlob :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Either (Int, L.ByteString) L.ByteString)
getBlob account authKey containerName blobName = do
    let resource = "/" +++ containerName +++ "/" +++ blobName
    Response status _ _ body <- doRequest account authKey resource [] "GET" "" []
    return $ if statusCode status >= 300 || statusCode status < 200 then Left (statusCode status, body) else Right body

breakLease :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Maybe (Int, L.ByteString))
breakLease account authKey containerName blobName = do
    let resource = "/" +++ containerName +++ "/" +++ blobName
    Response status _ _ body <- doRequest account authKey resource [("comp", "lease")] "PUT" "" [("x-ms-lease-action", "break")]
    return $ if statusCode status >= 300 || statusCode status < 200 then Just (statusCode status, body) else Nothing

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
                              , checkStatus = \_ _ -> Nothing -- don't throw an exception when a non-2xx error code is received
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

toLazy a = L.fromChunks [a]
toStrict = B.concat . L.toChunks