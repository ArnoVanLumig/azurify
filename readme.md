Azurify
=======

What's this?
------------

Azurify is an incomplete yet sort-of-functional library and command line client to access the Azure Blob Storage API [1]

The following features are implemented:
- Creating and deleting containers
- Listing the contents of a container
- Downloading blobs
- Uploading a new block blob *if it's no larger than 64MB*
- Deleting a blob
- Breaking a blob lease

The following features are *not* implemented (yet):
- Setting container and blob metadata
- Uploading blobs larger than 64MB
- Uploading page blobs
- Doing anything else with leases
- Anything with snapshots
- Proper error handling

How do I use this?
------------------

The cabal file will build a binary called ``azurify``. The following commands are supported:

- ``azurify uploadblob path/to/file accountname containername blobname``: to upload the local file ``path/to/file`` to the specified location;
- ``azurify downloadblob accountname containername blobname``: to download the specified blob the the current directory;
- ``azurify deleteblob accountname containername blobname``: to delete the specified blob;
- ``azurify breakbloblease accountname containername blobname``: to break the lease on the specified blob;
- ``azurify listcontainer accountname containername``: to output a list of blobs in the specified container;
- ``azurify createcontainer accountname containername [accesscontrol]``: accesscontrol may be either blobpublic, containerpublic or private. blobpublic means that the blobs will be downloadable by everyone, but the container can't be listed without an access key. containerpublic means that all blobs are downloadable by everyone and that everyone can list the container. private means that the access key is required to download blobs or list the container;
- ``azurify deletecontainer accountname containername``: delete the container with the given name. It will fail if the container is non-empty, if you want to delete it anyway use ``--force``.

You can also use the library in your Haskell applications, all you need are the functions exported in the ``azure.hs`` file.

Can I use this?
---------------

Azurify is BSD licensed (see LICENSE) so yes, you can use it.