#import <Cocoa/Cocoa.h>
#import "Window.h"
#import "HasCurrentFolder.h"
#import "HasSelectedInodes.h"
#import "Utilities.h"

@class WindowBrowserItem;
@interface WindowBrowser : Window
<NSOutlineViewDataSource, HasCurrentFolder, HasSelectedInodes>
{
    IBOutlet NSOutlineView *filesOutlineView;
    IBOutlet NSTableColumn *filesOutlineViewNameColumn;
    IBOutlet NSTableColumn *filesOutlineViewDateModifiedColumn;
    IBOutlet NSTableColumn *filesOutlineViewSizeColumn;
    IBOutlet NSTableColumn *filesOutlineViewKindColumn;
    IBOutlet NSTextField *itemCountLabel;
    
    BOOL ignoreItemExpansionDueToFixing;
    id ignoreItemExpansionDueToNesting;
    
    NSMapTable *browserItems;
}

- (id) initWithWindowID: (uuid_t *) newWindowID;
- (BOOL) getCurrentFolderInodeID: (uuid_t *) result;
- (void *) getSelectedInodeList;
- (WindowBrowserItem *) getBrowserItemWithInodeID: (uuid_t *) inodeID;
- (void) noteItemsChanged;
- (void) editItemName: (uuid_t *) inodeID;
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
- (void) fixItemExpansionState: (WindowBrowserItem *) item;
- (IBAction) doubleClickOutlineView: (id) sender;
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
