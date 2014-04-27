{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Te.HighLevel.DragAndDropPrivate
  (getDropTargetAndOperation,
   computeSameProject,
   getDragTargetInode,
   computeDragOperation)
  where

import Control.Exception
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word

import Te.LowLevel.Database
import Te.LowLevel.Exceptions
import Te.Types


getDropTargetAndOperation
    :: Inode
    -> DragInformation
    -> IO (Maybe (Inode, Maybe Word64, DragOperation))
getDropTargetAndOperation prospectiveTargetInode dragInformation = do
    case dragInformation of
      InodeDragInformation { } -> do
        let draggedInodes = inodeDragInformationInodes dragInformation
            sameProject =
              computeSameProject prospectiveTargetInode draggedInodes
            allowedDragOperations =
              dragInformationAllowedDragOperations dragInformation
        maybeTargetInode <-
          getDragTargetInode prospectiveTargetInode draggedInodes sameProject
        let maybeDragOperation =
              computeDragOperation allowedDragOperations sameProject
        case (maybeTargetInode, maybeDragOperation) of
          (Just targetInode, Just dragOperation) ->
            return $ Just (targetInode, Nothing, dragOperation)
          (Nothing, _) -> throwIO $(internalFailure)
          _ -> return Nothing
      _ -> return Nothing


computeSameProject :: Inode -> [Inode] -> Bool
computeSameProject prospectiveTargetInode draggedInodes =
  let targetProject = inodeProject prospectiveTargetInode
      sameProject = all (\draggedInode ->
                           let draggedProject = inodeProject draggedInode
                           in projectID draggedProject
                              == projectID targetProject)
                        draggedInodes
  in sameProject


getDragTargetInode :: Inode -> [Inode] -> Bool -> IO (Maybe Inode)
getDragTargetInode prospectiveTargetInode draggedInodes sameProject = do
  let considerInode prospectiveTargetInode = do
        inodeInformation <-
          lookupInodeInformation prospectiveTargetInode
        case inodeInformationType inodeInformation of
          DirectoryInodeType -> do
            if sameProject
               && any (\draggedInode ->
                         inodeID draggedInode
                         == inodeID prospectiveTargetInode)
                      draggedInodes
              then do
                maybeParentInode <- lookupInodeParent prospectiveTargetInode
                case maybeParentInode of
                  Just parentInode -> considerInode parentInode
                  Nothing -> return Nothing
              else return $ Just prospectiveTargetInode
          _ -> do
           maybeParentInode <- lookupInodeParent prospectiveTargetInode
           case maybeParentInode of
             Just parentInode -> considerInode parentInode
             Nothing -> return Nothing
  considerInode prospectiveTargetInode


computeDragOperation :: Set DragOperation -> Bool -> Maybe DragOperation
computeDragOperation allowedDragOperations sameProject =
  let dragOperationAllowed dragOperation =
        (Set.member dragOperation allowedDragOperations)
        || (Set.member DragOperationGeneric allowedDragOperations)
      maybeDragOperation =
        foldl' (\maybeResult (condition, operation) ->
                  case maybeResult of
                    Just _ -> maybeResult
                    Nothing -> if condition
                                  && dragOperationAllowed operation
                                 then Just operation
                                 else Nothing)
               Nothing
               [(sameProject, DragOperationMove),
                (True, DragOperationCopy)]
  in maybeDragOperation
