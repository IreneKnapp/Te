#import <Cocoa/Cocoa.h>
#import "Window.h"
#import "HasCurrentFolder.h"
#import "Utilities.h"


@class WindowDocumentView;
@interface WindowDocument : Window
<NSOutlineViewDataSource, HasCurrentFolder>
{
    WindowDocumentView *documentView;
    
    BOOL stillLoading;
    BOOL adjustingSize;
    NSSize manuallyAdjustedSize;
}
@property (assign) BOOL adjustingSize;

+ (NSUInteger) windowStyleMask;
- (id) initWithWindowID: (uuid_t *) newWindowID
       contentRectangle: (NSRect) contentRectangle;
- (void) setConstraints;
- (void) adjustSize: (NSSize) newSize withAnimation: (BOOL) withAnimation;
- (void) adjustSizePerContentConstraints;
- (void) windowWillStartLiveResize: (NSNotification *) notification;
- (void) windowDidEndLiveResize: (NSNotification *) notification;
- (void) windowDidBecomeMain: (NSNotification *) notification;
- (void) windowDidResignMain: (NSNotification *) notification;
- (void) preferredScrollerStyleDidChange: (NSNotification *) notification;
- (BOOL) getCurrentFolderInodeID: (uuid_t *) result;
- (void) mouseDown: (NSEvent *) event;
- (void) mouseDragged: (NSEvent *) event;
- (void) mouseUp: (NSEvent *) event;
@end
