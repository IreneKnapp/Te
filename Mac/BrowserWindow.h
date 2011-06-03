#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@class BrowserItem;
@interface BrowserWindow : NSWindowController
<NSWindowDelegate, NSOutlineViewDataSource>
{
    IBOutlet NSOutlineView *filesOutlineView;
    IBOutlet NSTableColumn *filesOutlineViewNameColumn;
    IBOutlet NSTableColumn *filesOutlineViewDateModifiedColumn;
    IBOutlet NSTableColumn *filesOutlineViewSizeColumn;
    IBOutlet NSTableColumn *filesOutlineViewKindColumn;
    IBOutlet NSTextField *itemCountLabel;
    
    BOOL alreadyClosing;
    
    NSMapTable *browserItems;
    uuid_t browserWindowID;
}

- (id) initWithBrowserWindowID: (uuid_t *) newBrowserWindowID;
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
@end
