module Te.HighLevel.Inode
  (Inode,
   InodeID,
   inodeID,
   inodeParent,
   inodeExpandable,
   inodeChildCount,
   inodeChild,
   inodeName,
   inodeKind,
   inodeSize,
   inodeCreationTimestamp,
   inodeModificationTimestamp,
   inodeIcon,
   inodeRename,
   inodeListDelete,
   inodeOpen,
   inodeValidateDrop,
   inodeAcceptDrop,
   inodesKernel)
  where

import Data.Word

import Data.ByteSize
import Data.Text (Text)
import Data.Timestamp
import Te.LowLevel.Identifiers
import Te.Types


inodeParent :: Inode -> IO (Maybe Inode)
inodeExpandable :: Inode -> IO Bool
inodeChildCount :: Inode -> IO Word64
inodeChild :: Inode -> Word64 -> IO Inode
inodeName :: Inode -> IO Text
inodeKind :: Inode -> IO Text
inodeSize :: Inode -> IO (Maybe ByteSize)
inodeCreationTimestamp :: Inode -> IO Timestamp
inodeModificationTimestamp :: Inode -> IO Timestamp
inodeIcon :: Inode -> IO Text
inodeRename :: Inode -> Text -> IO ()
inodeListDelete :: Maybe AnyWindow -> [Inode] -> IO ()
inodeOpen :: Inode -> IO ()
inodeValidateDrop
    :: Inode
    -> DragInformation
    -> IO (Maybe (Inode, Maybe Word64, DragOperation))
inodeAcceptDrop :: Inode -> DragInformation -> IO Bool
inodesKernel :: [Inode] -> IO [Inode]
