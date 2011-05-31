#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@interface BrowserWindow : NSWindowController
{
    IBOutlet NSOutlineView *outlineView;
    IBOutlet NSTableColumn *outlineViewNameColumn;
    IBOutlet NSTableColumn *outlineViewDateModifiedColumn;
    IBOutlet NSTableColumn *outlineViewSizeColumn;
    IBOutlet NSTableColumn *outlineViewKindColumn;
    IBOutlet NSTextField *itemCountLabel;
    
    uuid_t browserWindowID;
}

- (id) initWithBrowserWindowID: (uuid_t *) newBrowserWindowID;
@end
