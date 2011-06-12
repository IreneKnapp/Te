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
    
    BOOL stillLoading;
    BOOL adjustingSize;
    NSSize manuallyAdjustedSize;
}

- (id) initWithWindowID: (uuid_t *) newWindowID;
- (NSSize) defaultSize;
- (void) setConstraints;
- (void) adjustSize: (NSSize) newSize;
- (void) adjustSizePerContentConstraints;
- (void) windowWillStartLiveResize: (NSNotification *) notification;
- (void) windowDidEndLiveResize: (NSNotification *) notification;
- (BOOL) getCurrentFolderInodeID: (uuid_t *) result;
@end
