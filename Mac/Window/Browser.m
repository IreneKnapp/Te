#import "Window/Browser.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "Window/Browser/Item.h"
#import "FileNameCell.h"
#import "Utilities.h"


@implementation WindowBrowser

- (id) initWithWindowID: (uuid_t *) newWindowID {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return nil;
    
    self = [super initWithWindowID: newWindowID nibName: @"BrowserWindow"];
    if(self) {
        browserItems = [(AppDelegate *) [NSApp delegate] newMapTable];
        
        ignoreItemExpansionDueToFixing = NO;
        ignoreItemExpansionDueToNesting = nil;
                
        NSMutableArray *draggedTypes = [NSMutableArray arrayWithCapacity: 1];
        [draggedTypes addObject: @"com.dankna.te.datatypes.inodes"];
        [filesOutlineView registerForDraggedTypes: draggedTypes];
        
        uint64_t localOperations
            = teBrowserWindowDraggingSourceIntraApplicationOperations();
        NSDragOperation localOperationMask
            = dragOperationsToOperationMask(localOperations);
        [filesOutlineView setDraggingSourceOperationMask: localOperationMask
                          forLocal: YES];
        
        uint64_t remoteOperations
            = teBrowserWindowDraggingSourceInterApplicationOperations();
        NSDragOperation remoteOperationMask
            = dragOperationsToOperationMask(remoteOperations);
        [filesOutlineView setDraggingSourceOperationMask: remoteOperationMask
                          forLocal: NO];
        
        [filesOutlineView setDoubleAction:
                           @selector(doubleClickOutlineView:)];
        [filesOutlineView setTarget: self];
        
        [self noteItemsChanged];
    }
    return self;
}


- (BOOL) getCurrentFolderInodeID: (uuid_t *) result {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return NO;
    
    uuid_t *browserWindowID = [self windowID];
    NSIndexSet *rowIndices = [filesOutlineView selectedRowIndexes];
    if([rowIndices count] == 0) {
        teBrowserWindowRoot(applicationState, browserWindowID, result);
        return YES;
    } else {
        NSUInteger firstRow = [rowIndices firstIndex];
        id item = [filesOutlineView itemAtRow: firstRow];
        if([item isKindOfClass: [WindowBrowserItem class]]) {
            WindowBrowserItem *browserItemObject = (WindowBrowserItem *) item;
            
            uuid_t *inodeID = [browserItemObject inodeID];
            if(!teInodeParent(applicationState,
                              browserWindowID,
                              inodeID,
                              result))
            {
                copyUUID(result, inodeID);
            }
            return YES;
        } else {
            return NO;
        }
    }
}


- (void *) getSelectedInodeList {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return NULL;
    
    NSIndexSet *rowIndices = [filesOutlineView selectedRowIndexes];
    uint64_t inodeIDCount = [rowIndices count];
    if(inodeIDCount == 0) {
        return NULL;
    } else {
        __block uuid_t *inodeIDs = malloc(inodeIDCount * sizeof(uuid_t));
        
        __block uint64_t i = 0;
        [rowIndices enumerateIndexesUsingBlock:
                        ^(NSUInteger index, BOOL *stop)
                        {
                            id item = [filesOutlineView itemAtRow: index];
                            if([item isKindOfClass:
                                        [WindowBrowserItem class]])
                            {
                                WindowBrowserItem *browserItemObject
                                    = (WindowBrowserItem *) item;
                                
                                uuid_t *inodeID = [browserItemObject inodeID];
                                
                                copyUUID(&inodeIDs[i], inodeID);
                                
                                i++;
                            }
                        }];
        inodeIDCount = i;
        
        uuid_t *browserWindowID = [self windowID];
        
        void *inodeList = teInodeListNew(applicationState,
                                         browserWindowID,
                                         inodeIDCount,
                                         inodeIDs);
        
        free(inodeIDs);
        
        return inodeList;
    }
}


- (WindowBrowserItem *) getBrowserItemWithInodeID: (uuid_t *) inodeID {
    WindowBrowserItem *browserItem
        = [browserItems objectForKey: (void *) inodeID];
    if(!browserItem) {
        browserItem
            = [[WindowBrowserItem alloc] initWithWindowBrowserObject: self
                                         inodeID: inodeID];
        [browserItems setObject: browserItem
                      forKey: (void *) inodeID];
    }
    return browserItem;
}


- (void) noteItemsChanged {
    [filesOutlineView reloadData];
    [self fixItemExpansionState: nil];
}


- (void) editItemName: (uuid_t *) inodeID {
    // TODO - begin editing the name of the given item, as when it was just
    // created.
}


- (id) outlineView: (NSOutlineView *) outlineView
             child: (NSInteger) index
            ofItem: (id) item
{
    if([self alreadyClosing])
        return nil;
    
    if([outlineView isEqual: filesOutlineView]) {
        void *applicationState = getApplicationState();
        if(!applicationState)
            return nil;
        
        uuid_t *browserWindowID = [self windowID];
        
        uuid_t inodeChildID;
        
        if(!item) {
            uuid_t inodeID;
            teBrowserWindowRoot(applicationState,
                                browserWindowID,
                                &inodeID);
            
            teInodeChild(applicationState,
                         browserWindowID,
                         &inodeID,
                         index,
                         &inodeChildID);
        } else if([item isKindOfClass: [WindowBrowserItem class]]) {
            WindowBrowserItem *browserItem = (WindowBrowserItem *) item;
            
            uuid_t *inodeID = [browserItem inodeID];
            
            teInodeChild(applicationState,
                         browserWindowID,
                         inodeID,
                         index,
                         &inodeChildID);
        } else {
            return nil;
        }
        
        return [self getBrowserItemWithInodeID: &inodeChildID];
    } else {
        return nil;
    }
}


- (BOOL) outlineView: (NSOutlineView *) outlineView
    isItemExpandable: (id) item
{
    if([self alreadyClosing])
        return NO;
    
    if([outlineView isEqual: filesOutlineView]) {
        void *applicationState = getApplicationState();
        if(!applicationState)
            return NO;
        
        uuid_t *browserWindowID = [self windowID];
        
        if([item isKindOfClass: [WindowBrowserItem class]]) {
            WindowBrowserItem *browserItem = (WindowBrowserItem *) item;
            
            uuid_t *inodeID = [browserItem inodeID];
            uint64_t result
                = teInodeExpandable(applicationState,
                                    browserWindowID,
                                    inodeID);
            if(result)
                return YES;
            else
                return NO;
        } else {
            return NO;
        }
    } else {
        return NO;
    }
}


- (NSInteger) outlineView: (NSOutlineView *) outlineView
   numberOfChildrenOfItem: (id) item
{
    if([self alreadyClosing])
        return 0;
    
    if([outlineView isEqual: filesOutlineView]) {
        void *applicationState = getApplicationState();
        if(!applicationState)
            return 0;
        
        uuid_t *browserWindowID = [self windowID];
        
        if(!item) {
            uuid_t inodeID;
            teBrowserWindowRoot(applicationState,
                                browserWindowID,
                                &inodeID);
            
            uint64_t count
                = teInodeChildCount(applicationState,
                                    browserWindowID,
                                    &inodeID);
            return count;
        } else if([item isKindOfClass: [WindowBrowserItem class]]) {
            WindowBrowserItem *browserItem = (WindowBrowserItem *) item;
            
            uuid_t *inodeID = [browserItem inodeID];
            uint64_t count
                = teInodeChildCount(applicationState,
                                    browserWindowID,
                                    inodeID);
            return count;
        } else {
            return 0;
        }
    } else {
        return 0;
    }
}


- (id)          outlineView: (NSOutlineView *) outlineView
  objectValueForTableColumn: (NSTableColumn *) tableColumn
                     byItem: (id) item
{
    if([self alreadyClosing])
        return nil;
    
    if([outlineView isEqual: filesOutlineView]) {
        if([item isKindOfClass: [WindowBrowserItem class]]) {            
            void *applicationState = getApplicationState();
            if(!applicationState)
                return nil;
            
            uuid_t *browserWindowID = [self windowID];
            
            WindowBrowserItem *browserItemObject = (WindowBrowserItem *) item;
            uuid_t *inodeID = [browserItemObject inodeID];
            
            if([tableColumn isEqual: filesOutlineViewNameColumn]) {
                char *nameCString = teInodeName(applicationState,
                                                browserWindowID,
                                                inodeID);
                NSString *name = nil;
                if(nameCString) {
                    name = [NSString stringWithUTF8String: nameCString];
                    teStringFree(nameCString);
                }
                return name;
            } else if([tableColumn isEqual:
                                    filesOutlineViewDateModifiedColumn])
            {
                uint64_t timestamp
                    = teInodeModificationTimestamp(applicationState,
                                                   browserWindowID,
                                                   inodeID);
                char *timestampCString = teTimestampShow(timestamp);
                NSString *timestampString = nil;
                if(timestampCString) {
                    timestampString
                        = [NSString stringWithUTF8String: timestampCString];
                    teStringFree(timestampCString);
                }
                return timestampString;
            } else if([tableColumn isEqual: filesOutlineViewSizeColumn]) {
                uint64_t size;
                
                char *sizeCString = NULL;
                if(teInodeSize(applicationState,
                               browserWindowID,
                               inodeID,
                               &size))
                {
                    sizeCString = teByteSizeShow(size);
                } else {
                    sizeCString = teByteSizePlaceholderString();
                }
                NSString *sizeString = nil;
                if(sizeCString) {
                    sizeString = [NSString stringWithUTF8String: sizeCString];
                    teStringFree(sizeCString);
                }
                return sizeString;
            } else if([tableColumn isEqual: filesOutlineViewKindColumn]) {
                char *kindCString = teInodeKind(applicationState,
                                                browserWindowID,
                                                inodeID);
                NSString *kind = nil;
                if(kindCString) {
                    kind = [NSString stringWithUTF8String: kindCString];
                    teStringFree(kindCString);
                }
                return kind;
            } else {
                return nil;
            }
        } else {
            return nil;
        }
    } else {
        return nil;
    }
}


- (void) outlineView: (NSOutlineView *) outlineView
     willDisplayCell: (id) cell
      forTableColumn: (NSTableColumn *) tableColumn
                item: (id) item
{
    if([self alreadyClosing])
        return;
    
    if([outlineView isEqual: filesOutlineView]) {
        if([tableColumn isEqual: filesOutlineViewNameColumn]) {
            if([item isKindOfClass: [WindowBrowserItem class]]
               && [cell isKindOfClass: [FileNameCell class]])
            {
                void *applicationState = getApplicationState();
                if(!applicationState)
                    return;
                
                uuid_t *browserWindowID = [self windowID];
                
                WindowBrowserItem *browserItemObject
                    = (WindowBrowserItem *) item;
                FileNameCell *fileNameCell = (FileNameCell *) cell;
                
                uuid_t *inodeID = [browserItemObject inodeID];
                
                char *fileNameCString = teInodeName(applicationState,
                                                    browserWindowID,
                                                    inodeID);
                NSString *fileName = nil;
                if(fileNameCString) {
                    fileName
                        = [NSString stringWithUTF8String: fileNameCString];
                    teStringFree(fileNameCString);
                }
                if(!fileName)
                    fileName = @"";
                [fileNameCell setText: fileName];
                
                char *iconNameCString = teInodeIcon(applicationState,
                                                    browserWindowID,
                                                    inodeID);
                NSString *iconName = nil;
                if(iconNameCString) {
                    iconName = [NSString stringWithUTF8String: iconNameCString];
                    teStringFree(iconNameCString);
                }
                if(iconName) {
                    [fileNameCell setIcon: [NSImage imageNamed: iconName]];
                }
            }
        }
    }
}


- (void) outlineView: (NSOutlineView *) outlineView
      setObjectValue: (id) object
      forTableColumn: (NSTableColumn *) tableColumn
              byItem: (id) item
{
    if([self alreadyClosing])
        return;
    
    if([outlineView isEqual: filesOutlineView]) {
        if([tableColumn isEqual: filesOutlineViewNameColumn]) {
            if([item isKindOfClass: [WindowBrowserItem class]] &&
               [object isKindOfClass: [NSString class]])
            {
                void *applicationState = getApplicationState();
                if(!applicationState)
                    return;
                
                uuid_t *browserWindowID = [self windowID];
                
                WindowBrowserItem *browserItemObject
                    = (WindowBrowserItem *) item;
                NSString *newName = (NSString *) object;
                
                uuid_t *inodeID = [browserItemObject inodeID];
                
                char *newNameCString = (char *) [newName UTF8String];
                
                teInodeRename(applicationState,
                              browserWindowID,
                              inodeID,
                              newNameCString);
            }
        }
    }
}


- (void) outlineViewItemWillExpand: (NSNotification *) notification {
    if([self alreadyClosing])
        return;
    
    NSOutlineView *outlineView = [notification object];
    id item = [[notification userInfo] objectForKey: @"NSObject"];
    
    if([outlineView isEqual: filesOutlineView]) {
        if([item isKindOfClass: [WindowBrowserItem class]]) {
            WindowBrowserItem *browserItemObject = (WindowBrowserItem *) item;
            
            if(!ignoreItemExpansionDueToNesting)
                ignoreItemExpansionDueToNesting = item;
        }
    }
}


- (void) outlineViewItemWillCollapse: (NSNotification *) notification {
    if([self alreadyClosing])
        return;
    
    NSOutlineView *outlineView = [notification object];
    id item = [[notification userInfo] objectForKey: @"NSObject"];
    
    if([outlineView isEqual: filesOutlineView]) {
        if([item isKindOfClass: [WindowBrowserItem class]]) {
            WindowBrowserItem *browserItemObject = (WindowBrowserItem *) item;
            
            if(!ignoreItemExpansionDueToNesting)
                ignoreItemExpansionDueToNesting = item;
        }
    }
}


- (void) outlineViewItemDidExpand: (NSNotification *) notification {
    if([self alreadyClosing])
        return;
    
    NSOutlineView *outlineView = [notification object];
    id item = [[notification userInfo] objectForKey: @"NSObject"];
    
    if([outlineView isEqual: filesOutlineView]) {
        if(ignoreItemExpansionDueToNesting == item)
            ignoreItemExpansionDueToNesting = nil;
        
        if(ignoreItemExpansionDueToFixing)
            return;
        if(ignoreItemExpansionDueToNesting)
            return;
        
        void *applicationState = getApplicationState();
        if(!applicationState)
            return;
        
        uuid_t *browserWindowID = [self windowID];
        
        if([item isKindOfClass: [WindowBrowserItem class]]) {
            WindowBrowserItem *browserItemObject = (WindowBrowserItem *) item;
            
            uuid_t *inodeID = [browserItemObject inodeID];
            
            teBrowserItemSetExpanded(applicationState,
                                     browserWindowID,
                                     inodeID,
                                     1);
        }
        
        [self fixItemExpansionState: item];
    }
}


- (void) outlineViewItemDidCollapse: (NSNotification *) notification {
    if([self alreadyClosing])
        return;
    
    NSOutlineView *outlineView = [notification object];
    id item = [[notification userInfo] objectForKey: @"NSObject"];
    
    if([outlineView isEqual: filesOutlineView]) {
        if(ignoreItemExpansionDueToNesting == item)
            ignoreItemExpansionDueToNesting = nil;
        
        if(ignoreItemExpansionDueToFixing)
            return;
        if(ignoreItemExpansionDueToNesting)
            return;
        
        void *applicationState = getApplicationState();
        if(!applicationState)
            return;
        
        uuid_t *browserWindowID = [self windowID];
        
        if([item isKindOfClass: [WindowBrowserItem class]]) {
            WindowBrowserItem *browserItemObject = (WindowBrowserItem *) item;
            
            uuid_t *inodeID = [browserItemObject inodeID];
            
            teBrowserItemSetExpanded(applicationState,
                                     browserWindowID,
                                     inodeID,
                                     0);
        }
    }
}


- (void) fixItemExpansionState: (WindowBrowserItem *) item {
    if([self alreadyClosing])
        return;
    
    if(item) {
        if([item isKindOfClass: [WindowBrowserItem class]]) {
            void *applicationState = getApplicationState();
            if(!applicationState)
                return;
            
            uuid_t *browserWindowID = [self windowID];
            
            WindowBrowserItem *browserItemObject = (WindowBrowserItem *) item;
            
            uuid_t *inodeID = [browserItemObject inodeID];
            
            BOOL expanded = teBrowserItemExpanded(applicationState,
                                                  browserWindowID,
                                                  inodeID);
            
            ignoreItemExpansionDueToFixing = YES;
            if(expanded) {
                [filesOutlineView expandItem: item];
            } else {
                [filesOutlineView collapseItem: item];
            }
            ignoreItemExpansionDueToFixing = NO;
        }
    }
    
    if(!item || [filesOutlineView isItemExpanded: item]) {
        NSInteger childCount = [self outlineView: filesOutlineView
                                     numberOfChildrenOfItem: item];
        for(NSInteger i = 0; i < childCount; i++) {
            id childItem = [self outlineView: filesOutlineView
                                 child: i
                                 ofItem: item];
            [self fixItemExpansionState: childItem];
        }
    }
}


- (IBAction) doubleClickOutlineView: (id) sender {
    if([self alreadyClosing])
        return;
    
    if([sender isEqual: filesOutlineView]) {
        void *applicationState = getApplicationState();
        if(!applicationState)
            return;
        
        uuid_t *browserWindowID = [self windowID];
        
        NSInteger row = [filesOutlineView clickedRow];
        if(row == -1)
            return;
        
        id item = [filesOutlineView itemAtRow: row];
        if(![item isKindOfClass: [WindowBrowserItem class]])
            return;
        WindowBrowserItem *browserItemObject = (WindowBrowserItem *) item;
        
        uuid_t *inodeID = [browserItemObject inodeID];
        
        teInodeOpen(applicationState,
                    browserWindowID,
                    inodeID);
    }
}


- (BOOL) outlineView: (NSOutlineView *) outlineView
          writeItems: (NSArray *) items
        toPasteboard: (NSPasteboard *) pasteboard
{
    if([self alreadyClosing])
        return NO;
    
    if([outlineView isEqual: filesOutlineView]) {
        void *applicationState = getApplicationState();
        if(!applicationState)
            return NO;
        
        uuid_t *browserWindowID = [self windowID];
        
        NSMutableArray *declaredTypes = [NSMutableArray arrayWithCapacity: 1];
        [declaredTypes addObject: @"com.dankna.te.datatypes.inodes"];
        [pasteboard declareTypes: declaredTypes owner: self];
        
        BOOL result = YES;
        
        if(result) {
            NSMutableData *data = [NSMutableData data];
            
            [data appendBytes: browserWindowID length: 16];
            
            appendWord64(data, [items count]);
            
            for(WindowBrowserItem *item in items) {
                [data appendBytes: [item inodeID] length: 16];
            }
            
            result = [pasteboard
                       setData: data
                       forType: @"com.dankna.te.datatypes.inodes"];
        }
        
        return result;
    } else {
        return NO;
    }
}


- (NSDragOperation) outlineView: (NSOutlineView *) outlineView
                   validateDrop: (id <NSDraggingInfo>) info
                   proposedItem: (id) item
             proposedChildIndex: (NSInteger) index
{
    if([self alreadyClosing])
        return NSDragOperationNone;
    
    if([outlineView isEqual: filesOutlineView]) {
        void *applicationState = getApplicationState();
        if(!applicationState)
            return NSDragOperationNone;
        
        uuid_t *browserWindowID = [self windowID];
        
        uuid_t rootInodeID;
        teBrowserWindowRoot(applicationState, browserWindowID, &rootInodeID);
        
        uuid_t inodeID;
        if([item isKindOfClass: [WindowBrowserItem class]]) {
            WindowBrowserItem *browserItemObject = (WindowBrowserItem *) item;
            
            uuid_t *result = [browserItemObject inodeID];
            copyUUID(&inodeID, result);
        } else {
            copyUUID(&inodeID, &rootInodeID);
        }
        
        NSPasteboard *pasteboard = [info draggingPasteboard];
        
        NSString *usedType = nil;
        for(NSString *foundType in [pasteboard types]) {
            if([foundType isEqualToString: @"com.dankna.te.datatypes.inodes"])
            {
                usedType = foundType;
                break;
            }
        }
        if(!usedType)
            return NSDragOperationNone;
        
        NSDragOperation allowedDragOperationMask
            = [info draggingSourceOperationMask];
        uint64_t allowedDragOperations
            = dragOperationMaskToOperations(allowedDragOperationMask);
        
        void *dragInformation = NULL;
        if([usedType isEqualToString: @"com.dankna.te.datatypes.inodes"])
        {
            NSData *data
                = [pasteboard dataForType: @"com.dankna.te.datatypes.inodes"];
            dragInformation
                = extractInodesDragInformation(data, allowedDragOperations);
        }
        if(!dragInformation) {
            teFrontEndInternalFailure(applicationState, __FILE__, __LINE__);
            return NSDragOperationNone;
        }
        
        uuid_t resultInodeID;
        uint64_t resultChildIndex;
        uint64_t resultDragOperation;
        uint64_t resultType = teInodeValidateDrop(applicationState,
                                                  browserWindowID,
                                                  &inodeID,
                                                  dragInformation,
                                                  &resultInodeID,
                                                  &resultChildIndex,
                                                  &resultDragOperation);
        
        teDragInformationFree(dragInformation);
        
        NSInteger dropChildIndex;
        switch(resultType) {
        case 0:
        default:
            return NSDragOperationNone;
        case 1:
            dropChildIndex = NSOutlineViewDropOnItemIndex;
            break;
        case 2:
            dropChildIndex = resultChildIndex;
            break;
        }
        WindowBrowserItem *browserItem = nil;
        if(!teUUIDEqual(&resultInodeID, &rootInodeID))
            browserItem = [self getBrowserItemWithInodeID: &resultInodeID];
        [filesOutlineView setDropItem: browserItem
                          dropChildIndex: dropChildIndex];
        
        NSDragOperation resultDragOperationMask
            = dragOperationsToOperationMask(resultDragOperation);
        return resultDragOperationMask;
    } else {
        return NSDragOperationNone;
    }
}


- (BOOL) outlineView: (NSOutlineView *) outlineView
          acceptDrop: (id <NSDraggingInfo>) info
                item: (id) item
          childIndex: (NSInteger) index
{
    if([self alreadyClosing])
        return NO;
    
    if([outlineView isEqual: filesOutlineView]) {
        void *applicationState = getApplicationState();
        if(!applicationState)
            return NO;
        
        uuid_t *browserWindowID = [self windowID];
        
        uuid_t rootInodeID;
        teBrowserWindowRoot(applicationState, browserWindowID, &rootInodeID);
        
        uuid_t inodeID;
        if([item isKindOfClass: [WindowBrowserItem class]]) {
            WindowBrowserItem *browserItemObject = (WindowBrowserItem *) item;
            
            uuid_t *result = [browserItemObject inodeID];
            copyUUID(&inodeID, result);
        } else {
            copyUUID(&inodeID, &rootInodeID);
        }
        
        NSPasteboard *pasteboard = [info draggingPasteboard];
        
        NSString *usedType = nil;
        for(NSString *foundType in [pasteboard types]) {
            if([foundType isEqualToString: @"com.dankna.te.datatypes.inodes"])
            {
                usedType = foundType;
                break;
            }
        }
        if(!usedType)
            return NO;
        
        NSDragOperation allowedDragOperationMask
            = [info draggingSourceOperationMask];
        uint64_t allowedDragOperations
            = dragOperationMaskToOperations(allowedDragOperationMask);
        
        void *dragInformation = NULL;
        if([usedType isEqualToString: @"com.dankna.te.datatypes.inodes"])
        {
            NSData *data
                = [pasteboard dataForType: @"com.dankna.te.datatypes.inodes"];
            dragInformation
                = extractInodesDragInformation(data, allowedDragOperations);
        }
        if(!dragInformation) {
            teFrontEndInternalFailure(applicationState, __FILE__, __LINE__);
            return NO;
        }
        
        uint64_t result = teInodeAcceptDrop(applicationState,
                                            browserWindowID,
                                            &inodeID,
                                            dragInformation);
        
        teDragInformationFree(dragInformation);
        
        if(result == 0)
            return NO;
        else
            return YES;
    } else {
        return NO;
    }
}

@end
