#import "TransparentHelperWindow.h"
#import "TransparentHelperWindowView.h"


@implementation TransparentHelperWindow

- (id) initWithContentRect: (NSRect) contentRect
                drawHelper: (void (^)(NSRect drawFrame)) newDrawHelper
               aboveWindow: (NSWindow *) aboveWindow
{
    contentRect.origin
        = [aboveWindow convertBaseToScreen: contentRect.origin];
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
    newLocation = [[self parentWindow] convertBaseToScreen: newLocation];
    savedMouseLocation = newLocation;
    axes = newAxes;
}


- (void) updateMouse: (NSPoint) newLocation {
    newLocation = [[self parentWindow] convertBaseToScreen: newLocation];
    
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


- (void) startTrackingView: (NSView *) newTrackedView
              resizingMask: (NSUInteger) newTrackedViewResizingMask
{
    trackedView = newTrackedView;
    trackedViewResizingMask = newTrackedViewResizingMask;
    
    trackedViewOldFrame = [trackedView frame];
    trackedViewOldFrame = [trackedView convertRectToBase: trackedViewOldFrame];
    trackedViewOldFrame.origin
        = [[trackedView window] convertBaseToScreen:
                                 trackedViewOldFrame.origin];
    
    [trackedView setPostsFrameChangedNotifications: YES];
    
    NSNotificationCenter *notificationCenter
        = [NSNotificationCenter defaultCenter];
    [notificationCenter addObserver: self
                        selector: @selector(updateTrackedView:)
                        name: NSViewFrameDidChangeNotification
                        object: trackedView];
}


- (void) updateTrackedView: (NSNotification *) notification {
    NSRect trackedViewNewFrame = [trackedView frame];
    trackedViewNewFrame = [trackedView convertRectToBase: trackedViewNewFrame];
    trackedViewNewFrame.origin
        = [[trackedView window] convertBaseToScreen:
                                 trackedViewNewFrame.origin];
    
    if(trackedViewResizingMask == NSViewNotSizable) {
        trackedViewOldFrame = trackedViewNewFrame;
        return;
    }
    
    NSRect frame = [self frame];
    
    if(trackedViewResizingMask & NSViewWidthSizable) {
        frame.size.width
            += trackedViewNewFrame.size.width - trackedViewOldFrame.size.width;
    }
    
    if(!(trackedViewResizingMask & NSViewMinXMargin)) {
        frame.origin.x
            += trackedViewNewFrame.origin.x - trackedViewOldFrame.origin.x;
    } else if(!(trackedViewResizingMask & NSViewMaxXMargin)) {
        CGFloat oldRight
            = trackedViewOldFrame.origin.x + trackedViewOldFrame.size.width;
        CGFloat newRight
            = trackedViewNewFrame.origin.x + trackedViewNewFrame.size.width;
        frame.origin.x += newRight - oldRight;
    }
    
    if(trackedViewResizingMask & NSViewHeightSizable) {
        frame.size.height += trackedViewNewFrame.size.height
                             - trackedViewOldFrame.size.height;
    }
    
    if(!(trackedViewResizingMask & NSViewMinYMargin)) {
        frame.origin.y
            += trackedViewNewFrame.origin.y - trackedViewOldFrame.origin.y;
    } else if(!(trackedViewResizingMask & NSViewMaxYMargin)) {
        CGFloat oldTop
            = trackedViewOldFrame.origin.y + trackedViewOldFrame.size.height;
        CGFloat newTop
            = trackedViewNewFrame.origin.y + trackedViewNewFrame.size.height;
        frame.origin.y += newTop - oldTop;
    }
    
    [self setFrame: frame display: NO];
    
    trackedViewOldFrame = trackedViewNewFrame;
}

@end
