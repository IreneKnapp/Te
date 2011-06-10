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
        
        NSView *contentView = [self contentView];
        NSRect viewFrame = [contentView frame];
        NSView *drawingView
            = [[TransparentHelperWindowView alloc] initWithFrame: viewFrame
                                                   drawHelper: newDrawHelper];
        [contentView addSubview: drawingView];
        
        [self setOpaque: NO];
        [self setIgnoresMouseEvents: YES];
        [self setAlphaValue: 0.25];
        
        [aboveWindow addChildWindow: self ordered: NSWindowAbove];
    }
    return self;
}


- (void) remove {
    [[self parentWindow] removeChildWindow: self];
    [self orderOut: self];
}


- (void) startTrackingMouse: (NSPoint) newLocation
                     onAxes: (enum Axes) newAxes
{
    newLocation = [[self parentWindow] convertBaseToScreen: newLocation];
    savedMouseLocation = newLocation;
    axes = newAxes;
}


- (void) updateMouse: (NSPoint) newLocation {
    newLocation = [[self parentWindow] convertBaseToScreen: newLocation];
    
    NSSize offset = NSZeroSize;
    if((axes == BothAxes) || (axes == HorizontalAxis))
        offset.width = newLocation.x - savedMouseLocation.x;
    if((axes == BothAxes) || (axes == VerticalAxis))
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
