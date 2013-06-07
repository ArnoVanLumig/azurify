{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import qualified Azure as Az
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import System.Console.CmdArgs
import System.Directory(getCurrentDirectory)

data Commands = UploadBlob { uploadBlobPath :: String
                           , uploadBlobStorageName :: String
                           , uploadBlobContainerName :: String
                           , uploadBlobFileName :: String
                           , uploadBlobContentType :: Maybe String
                           , uploadBlobContentEncoding :: Maybe String
                           , uploadBlobContentLanguage :: Maybe String
                           , uploadBlobContentCache :: Maybe String
                           }
              | DeleteBlob { deleteBlobStorageName :: String
                           , deleteBlobContainerName :: String
                           , deleteBlobBlobName :: String
                           }
              | DownloadBlob { downloadBlobStorageName :: String
                             , downloadBlobContainerName :: String
                             , downloadBlobBlobName :: String
                             }
              | CreateContainer { createContainerStorageName :: String
                                , createContainerContainerName :: String
                                , createContainerACL :: Maybe String
                                }
              | DeleteContainer { deleteContainerStorageName :: String
                                , deleteContainerContainerName :: String
                                , deleteContainerForce :: Maybe Bool
                                }
              | ListContainer { listContainerStorageName :: String
                              , listContainerContainerName :: String
                              }
              | BreakBlobLease { breakLeaseStorageName :: String
                               , breakLeaseContainerName :: String
                               , breakLeaseBlobName :: String
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
                        } &= help "Upload a blob"

downloadBlob = DownloadBlob { downloadBlobStorageName = def &= typ "accountname" &= argPos 0
                            , downloadBlobContainerName = def &= typ "containername" &= argPos 1
                            , downloadBlobBlobName = def &= typ "blobname" &= argPos 2
                            } &= help "Download a blob"

deleteBlob = DeleteBlob { deleteBlobStorageName = def &= typ "accountname" &= argPos 0
                        , deleteBlobContainerName = def &= typ "containername" &= argPos 1
                        , deleteBlobBlobName = def &= typ "blobname" &= argPos 2
                        } &= help "Delete a blob"

breakBlobLease = BreakBlobLease { breakLeaseStorageName = def &= typ "accountname" &= argPos 0
                                , breakLeaseContainerName = def &= typ "containername" &= argPos 1
                                , breakLeaseBlobName = def &= typ "blobname" &= argPos 2
                                }

listContainer = ListContainer { listContainerStorageName = def &= typ "accountname" &= argPos 0
                              , listContainerContainerName = def &= typ "containername" &= argPos 1
                              } &= help "List all blobs in a container"

createContainer = CreateContainer { createContainerStorageName = def &= typ "accountname" &= argPos 0
                                  , createContainerContainerName = def &= typ "containername" &= argPos 1
                                  , createContainerACL = def &= typ "blobpublic|containerpublic|private" &= argPos 2
                                  } &= help "Create a container with the specified access control"

deleteContainer = DeleteContainer { deleteContainerStorageName = def &= typ "accountname" &= argPos 0
                                  , deleteContainerContainerName = def &= typ "containername" &= argPos 1
                                  , deleteContainerForce = def &= name "force"
                                  } &= help "Delete the container and all the blobs inside it"

mode = cmdArgsMode $ modes [uploadBlob, downloadBlob, deleteBlob, breakBlobLease, listContainer, createContainer, deleteContainer] &= help "Access the Azure blob storage" &= program "azurify" &= summary "Azurify v1.0"

main :: IO ()
main = do
    m <- cmdArgsRun mode
    putStrLn "Please enter your authkey:"
    azureKey <- B.getLine
    case m of
        UploadBlob path account container name contType contEnc contLang contCache -> do
            contents <- B.readFile path
            res <- Az.createBlob (B8.pack account)
                                 azureKey (B8.pack container)
                                 (Az.BlobSettings (B8.pack name)
                                                  (B8.pack `fmap` contType)
                                                  (B8.pack `fmap` contEnc)
                                                  (B8.pack `fmap` contLang)
                                                  Nothing
                                                  (B8.pack `fmap` contCache)
                                                  Az.BlockBlob
                                                  Nothing
                                                  (Just contents)
                                                  )
            case res of
                Just (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                Nothing -> return ()
        DownloadBlob account container blobname -> do -- TODO: progress indicator
            pwd <- getCurrentDirectory
            res <- Az.getBlob (B8.pack account) azureKey (B8.pack container) (B8.pack blobname)
            let path = pwd ++ "/" ++ blobname
            putStrLn path
            case res of
                Left (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                Right content -> L.writeFile path content
        DeleteBlob account container blobname -> do
            res <- Az.deleteBlob (B8.pack account) azureKey (B8.pack container) (B8.pack blobname)
            case res of
                Just (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                _ -> return ()
        BreakBlobLease account container blobname -> do
            res <- Az.breakLease (B8.pack account) azureKey (B8.pack container) (B8.pack blobname)
            case res of
                Just (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                _ -> return ()
        ListContainer account container -> do -- TODO: output formatting
            res <- Az.listContainer (B8.pack account) azureKey (B8.pack container)
            case res of
                Left (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                Right blobs -> mapM_ print blobs
        CreateContainer account container access -> do
            let acl = case access of {
                Just "blobpublic" -> Az.BlobPublic;
                Just "containerpublic" -> Az.ContainerPublic;
                Just "private" -> Az.Private;
                Nothing -> Az.Private;
                _ -> error "invalid access control specified";
                }
            res <- Az.createContainer (B8.pack account) azureKey (B8.pack container) acl
            case res of
                Just (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                Nothing -> return ()
        DeleteContainer account container force -> do
            if force == (Just True) then do
                res <- Az.listContainer (B8.pack account) azureKey (B8.pack container)
                case res of
                    Left (stat, err) -> putStrLn "error listing container" >> print stat >> putStrLn "\n" >> print err
                    Right (x:_) -> error "Container not empty, use --force to ignore"
                else do
                    res <- Az.deleteContainer (B8.pack account) azureKey (B8.pack container)
                    case res of
                        Just (stat, err) -> putStrLn "error" >> print stat >> putStrLn "\n" >> print err
                        Nothing -> return ()


