module Te.HighLevel.DragAndDrop
  (browserWindowDraggingSourceIntraApplicationOperations,
   browserWindowDraggingSourceInterApplicationOperations)
  where

import Data.Set (Set)
import qualified Data.Set as Set

import Te.Types


browserWindowDraggingSourceIntraApplicationOperations :: Set DragOperation
browserWindowDraggingSourceIntraApplicationOperations =
  Set.union browserWindowDraggingSourceInterApplicationOperations
            $ Set.fromList [DragOperationLink,
                            DragOperationMove]


browserWindowDraggingSourceInterApplicationOperations :: Set DragOperation
browserWindowDraggingSourceInterApplicationOperations =
  Set.fromList [DragOperationCopy,
                DragOperationGeneric,
                DragOperationDelete]
