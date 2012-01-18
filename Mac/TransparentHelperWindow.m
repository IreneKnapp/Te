#import "TransparentHelperWindow.h"
#import "TransparentHelperWindowView.h"


@implementation TransparentHelperWindow

- (id) initWithContentRect: (NSRect) contentRect
                drawHelper: (void (^)(NSRect drawFrame)) newDrawHelper
               aboveWindow: (NSWindow *) aboveWindow
{
    self = [super initWithContentRect: contentRect
                  styleMask: NSBorderlessWindowMask
                  backing: NSBackingStoreBuffered
                  defer: NO];
    if(self) {
        savedMouseLocation = NSZeroPoint;
        trackedView = nil;
        trackedViewResizingMask = 0;
        
        NSView *contentView = [self contentView];
        NSRect viewFrame = [contentView frame];
        NSView *drawingView
            = [[TransparentHelperWindowView alloc] initWithFrame: viewFrame
                                                   drawHelper: newDrawHelper];
        [contentView addSubview: drawingView];
        
        [self setOpaque: NO];
        [self setIgnoresMouseEvents: YES];
        [self setAlphaValue: 0.50];
        
        [aboveWindow addChildWindow: self ordered: NSWindowAbove];
    }
    return self;
}


- (void) remove {
    [[self parentWindow] removeChildWindow: self];
    
    NSNotificationCenter *notificationCenter
        = [NSNotificationCenter defaultCenter];
    [notificationCenter removeObserver: self];
    
    if(trackedView) {
        [trackedView setPostsFrameChangedNotifications: NO];
    }
    
    [self orderOut: self];
}


- (void) startTrackingMouse: (NSPoint) newLocation
                     onAxes: (enum MouseTrackingAxes) newAxes
{
    savedMouseLocation = newLocation;
    axes = newAxes;
}


- (void) updateMouse: (NSPoint) newLocation {
    NSSize offset = NSZeroSize;
    if((axes == TrackMouseBothAxes) || (axes == TrackMouseHorizontalAxis))
        offset.width = newLocation.x - savedMouseLocation.x;
    if((axes == TrackMouseBothAxes) || (axes == TrackMouseVerticalAxis))
        offset.height = newLocation.y - savedMouseLocation.y;
    
    [self offsetBy: offset];
    
    savedMouseLocation = newLocation;
}


- (void) offsetBy: (NSSize) offsetAmount {
    NSPoint origin = [self frame].origin;
    origin.x += offsetAmount.width;
    origin.y += offsetAmount.height;
    [self setFrameOrigin: origin];
}

@end
