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
@property (assign) BOOL adjustingSize;

- (id) initWithWindowID: (uuid_t *) newWindowID;
- (NSSize) defaultSize;
- (void) setConstraints;
- (void) adjustSize: (NSSize) newSize withAnimation: (BOOL) withAnimation;
- (void) adjustSizePerContentConstraints;
- (void) showResizingTips;
- (void) hideResizingTips;
- (void) updateResizingTips;
- (void) windowWillStartLiveResize: (NSNotification *) notification;
- (void) windowDidEndLiveResize: (NSNotification *) notification;
- (void) windowDidBecomeMain: (NSNotification *) notification;
- (void) windowDidResignMain: (NSNotification *) notification;
- (void) preferredScrollerStyleDidChange: (NSNotification *) notification;
- (BOOL) getCurrentFolderInodeID: (uuid_t *) result;
@end
