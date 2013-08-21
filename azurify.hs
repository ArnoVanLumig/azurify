{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Azure as Az
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import System.Console.CmdArgs
import System.Directory(getCurrentDirectory)
import System.PosixCompat.Files (getFileStatus, fileSize)

data Commands = UploadBlob { uploadBlobPath :: String
                           , uploadBlobStorageName :: String
                           , uploadBlobContainerName :: String
                           , uploadBlobFileName :: String
                           , uploadBlobContentType :: Maybe String
                           , uploadBlobContentEncoding :: Maybe String
                           , uploadBlobContentLanguage :: Maybe String
                           , uploadBlobContentCache :: Maybe String
                           , accessKey :: String
                           }
              | DeleteBlob { deleteBlobStorageName :: String
                           , deleteBlobContainerName :: String
                           , deleteBlobBlobName :: String
                           , accessKey :: String
                           }
              | DownloadBlob { downloadBlobStorageName :: String
                             , downloadBlobContainerName :: String
                             , downloadBlobBlobName :: String
                             , accessKey :: String
                             }
              | CreateContainer { createContainerStorageName :: String
                                , createContainerContainerName :: String
                                , createContainerACL :: Maybe String
                                , accessKey :: String
                                }
              | DeleteContainer { deleteContainerStorageName :: String
                                , deleteContainerContainerName :: String
                                , deleteContainerForce :: Maybe Bool
                                , accessKey :: String
                                }
              | ListContainer { listContainerStorageName :: String
                              , listContainerContainerName :: String
                              , accessKey :: String
                              }
              | BreakBlobLease { breakLeaseStorageName :: String
                               , breakLeaseContainerName :: String
                               , breakLeaseBlobName :: String
                               , accessKey :: String
                               }
              deriving (Show, Data, Typeable, Eq)

uploadBlob = UploadBlob { uploadBlobPath          = def &= typ "file" &= argPos 0
                        , uploadBlobStorageName   = def &= typ "accountname" &= argPos 1
                        , uploadBlobContainerName = def &= typ "containername" &= argPos 2
                        , uploadBlobFileName      = def &= typ "blobname" &= argPos 3
                        , uploadBlobContentType   = def &= name "contenttype"
                        , uploadBlobContentEncoding = def &= name "contentencoding"
                        , uploadBlobContentLanguage = def &= name "contentlanguage"
                        , uploadBlobContentCache  = def &= name "cachecontrol"
                        , accessKey = def &= name "accesskey"
                        } &= help "Upload a blob"

downloadBlob = DownloadBlob { downloadBlobStorageName = def &= typ "accountname" &= argPos 0
                            , downloadBlobContainerName = def &= typ "containername" &= argPos 1
                            , downloadBlobBlobName = def &= typ "blobname" &= argPos 2
                            , accessKey = def &= name "accesskey"
                            } &= help "Download a blob"

deleteBlob = DeleteBlob { deleteBlobStorageName = def &= typ "accountname" &= argPos 0
                        , deleteBlobContainerName = def &= typ "containername" &= argPos 1
                        , deleteBlobBlobName = def &= typ "blobname" &= argPos 2
                        , accessKey = def &= name "accesskey"
                        } &= help "Delete a blob"

breakBlobLease = BreakBlobLease { breakLeaseStorageName = def &= typ "accountname" &= argPos 0
                                , breakLeaseContainerName = def &= typ "containername" &= argPos 1
                                , breakLeaseBlobName = def &= typ "blobname" &= argPos 2
                                , accessKey = def &= name "accesskey"
                                }

#ifndef NO_XML
listContainer = ListContainer { listContainerStorageName = def &= typ "accountname" &= argPos 0
                              , listContainerContainerName = def &= typ "containername" &= argPos 1
                              , accessKey = def &= name "accesskey"
                              } &= help "List all blobs in a container"
#endif

createContainer = CreateContainer { createContainerStorageName = def &= typ "accountname" &= argPos 0
                                  , createContainerContainerName = def &= typ "containername" &= argPos 1
                                  , createContainerACL = def &= typ "blobpublic|containerpublic|private" &= argPos 2
                                  , accessKey = def &= name "accesskey"
                                  } &= help "Create a container with the specified access control"

#ifndef NO_XML
deleteContainer = DeleteContainer { deleteContainerStorageName = def &= typ "accountname" &= argPos 0
                                  , deleteContainerContainerName = def &= typ "containername" &= argPos 1
                                  , deleteContainerForce = def &= name "force"
                                  , accessKey = def &= name "accesskey"
                                  } &= help "Delete the container and all the blobs inside it"
#endif

mode = cmdArgsMode $ modes [uploadBlob, downloadBlob, deleteBlob, breakBlobLease
#ifndef NO_XML
         , listContainer
#endif
         , createContainer
#ifndef NO_XML
         , deleteContainer
#endif
         ] &= help "Access the Azure blob storage" &= program "azurify" &= summary "Azurify v1.0"

main :: IO ()
main = do
    m <- cmdArgsRun mode
    case m of
        UploadBlob path account container name contType contEnc contLang contCache azureKey -> do
            size <- fileSize `fmap` getFileStatus path
            settings <- if size < 1028 * 1028 * 64
              then do contents <- B.readFile path
                      return $ Az.BlockBlobSettings (B8.pack name) contents
              else    return $ Az.FileBlobSettings (B8.pack name) path
            res <- Az.createBlob (B8.pack account) (B8.pack azureKey) (B8.pack container)
                       $ settings $ Az.BlobSettings
                            (B8.pack `fmap` contType)
                            (B8.pack `fmap` contEnc)
                            (B8.pack `fmap` contLang)
                            Nothing
                            (B8.pack `fmap` contCache)
                            []
            case res of
                Just (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                Nothing -> return ()
        DownloadBlob account container blobname azureKey -> do -- TODO: progress indicator
            pwd <- getCurrentDirectory
            res <- Az.getBlob (B8.pack account) (B8.pack azureKey) (B8.pack container) (B8.pack blobname)
            let path = pwd ++ "/" ++ blobname
            putStrLn path
            case res of
                Left (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                Right content -> L.writeFile path content
        DeleteBlob account container blobname azureKey -> do
            res <- Az.deleteBlob (B8.pack account) (B8.pack azureKey) (B8.pack container) (B8.pack blobname)
            case res of
                Just (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                _ -> return ()
        BreakBlobLease account container blobname azureKey -> do
            res <- Az.breakLease (B8.pack account) (B8.pack azureKey) (B8.pack container) (B8.pack blobname)
            case res of
                Just (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                _ -> return ()
#ifndef NO_XML
        ListContainer account container azureKey -> do -- TODO: output formatting
            res <- Az.listContainer (B8.pack account) (B8.pack azureKey) (B8.pack container)
            case res of
                Left (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                Right blobs -> mapM_ print blobs
#endif
        CreateContainer account container access azureKey -> do
            let acl = case access of {
                Just "blobpublic" -> Az.BlobPublic;
                Just "containerpublic" -> Az.ContainerPublic;
                Just "private" -> Az.Private;
                Nothing -> Az.Private;
                _ -> error "invalid access control specified";
                }
            res <- Az.createContainer (B8.pack account) (B8.pack azureKey) (B8.pack container) acl
            case res of
                Just (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                Nothing -> return ()
#ifndef NO_XML
        DeleteContainer account container force azureKey -> do
            if force == (Just True) then do
                res <- Az.listContainer (B8.pack account) (B8.pack azureKey) (B8.pack container)
                case res of
                    Left (stat, err) -> putStrLn "error listing container" >> print stat >> putStrLn "\n" >> print err
                    Right (x:_) -> error "Container not empty, use --force to ignore"
                else do
                    res <- Az.deleteContainer (B8.pack account) (B8.pack azureKey) (B8.pack container)
                    case res of
                        Just (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                        Nothing -> return ()
#endif
