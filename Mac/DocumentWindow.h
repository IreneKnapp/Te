#import <Cocoa/Cocoa.h>
#import "Window.h"
#import "HasCurrentFolder.h"
#import "Utilities.h"


@class DocumentSplitView;
@class DocumentContentView;
@interface DocumentWindow : Window
<NSOutlineViewDataSource, HasCurrentFolder>
{
    IBOutlet NSTextField *numberOfLinesLabel;
    DocumentSplitView *documentSplitView;
}

- (id) initWithWindowID: (uuid_t *) newWindowID;
- (void) sizeToDefault;
- (void) setConstraints;
- (void) adjustSize: (NSSize) newSize;
- (BOOL) getCurrentFolderInodeID: (uuid_t *) result;
@end
