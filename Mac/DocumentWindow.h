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
    DocumentContentView *firstDocumentContentView;
}

- (id) initWithWindowID: (uuid_t *) newWindowID;
- (BOOL) getCurrentFolderInodeID: (uuid_t *) result;
@end
