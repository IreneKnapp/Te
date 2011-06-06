#import <Cocoa/Cocoa.h>
#import "HasCurrentFolder.h"
#import "Utilities.h"

@class BrowserItem;
@interface BrowserWindow : NSWindowController
<NSWindowDelegate, NSOutlineViewDataSource, HasCurrentFolder>
{
    IBOutlet NSOutlineView *filesOutlineView;
    IBOutlet NSTableColumn *filesOutlineViewNameColumn;
    IBOutlet NSTableColumn *filesOutlineViewDateModifiedColumn;
    IBOutlet NSTableColumn *filesOutlineViewSizeColumn;
    IBOutlet NSTableColumn *filesOutlineViewKindColumn;
    IBOutlet NSTextField *itemCountLabel;
    
    BOOL alreadyClosing;
    BOOL ignoreItemExpansionDueToFixing;
    id ignoreItemExpansionDueToNesting;
    
    NSMapTable *browserItems;
    uuid_t browserWindowID;
}

- (id) initWithBrowserWindowID: (uuid_t *) newBrowserWindowID;
- (uuid_t *) browserWindowID;
- (BOOL) getCurrentFolderInodeID: (uuid_t *) result;
- (BrowserItem *) getBrowserItemWithInodeID: (uuid_t *) inodeID;
- (void) forceClose;
- (void) noteItemsChanged;
- (void) editItemName: (uuid_t *) inodeID;
- (BOOL) windowShouldClose: (id) sender;
- (void) windowWillClose: (NSNotification *) notification;
- (BOOL)               window: (NSWindow *) window
  shouldDragDocumentWithEvent: (NSEvent *) event
                         from: (NSPoint) dragImageLocation
               withPasteboard: (NSPasteboard *) pasteboard;
- (BOOL)               window: (NSWindow *) window
  shouldPopUpDocumentPathMenu: (NSMenu *) menu;
- (id) outlineView: (NSOutlineView *) outlineView
             child: (NSInteger) index
            ofItem: (id) item;
- (BOOL) outlineView: (NSOutlineView *) outlineView
    isItemExpandable: (id) item;
- (NSInteger) outlineView: (NSOutlineView *) outlineView
   numberOfChildrenOfItem: (id) item;
- (id)          outlineView: (NSOutlineView *) outlineView
  objectValueForTableColumn: (NSTableColumn *) tableColumn
                     byItem: (id) item;
- (void) outlineView: (NSOutlineView *) outlineView
     willDisplayCell: (id) cell
      forTableColumn: (NSTableColumn *) tableColumn
                item: (id) item;
- (void) outlineView: (NSOutlineView *) outlineView
      setObjectValue: (id) object
      forTableColumn: (NSTableColumn *) tableColumn
              byItem: (id) item;
- (void) outlineViewItemWillExpand: (NSNotification *) notification;
- (void) outlineViewItemWillCollapse: (NSNotification *) notification;
- (void) outlineViewItemDidExpand: (NSNotification *) notification;
- (void) outlineViewItemDidCollapse: (NSNotification *) notification;
- (void) fixItemExpansionState: (BrowserItem *) item;
- (BOOL) outlineView: (NSOutlineView *) outlineView
          writeItems: (NSArray *) items
        toPasteboard: (NSPasteboard *) pasteboard;
- (NSDragOperation) outlineView: (NSOutlineView *) outlineView
                   validateDrop: (id <NSDraggingInfo>) info
                   proposedItem: (id) item
             proposedChildIndex: (NSInteger) index;
- (BOOL) outlineView: (NSOutlineView *) outlineView
          acceptDrop: (id <NSDraggingInfo>) info
                item: (id) item
          childIndex: (NSInteger) index;
@end
