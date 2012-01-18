#import <Cocoa/Cocoa.h>


enum MouseTrackingAxes {
  TrackMouseBothAxes,
  TrackMouseHorizontalAxis,
  TrackMouseVerticalAxis
};

@interface TransparentHelperWindow : NSWindow
{
    NSPoint savedMouseLocation;
    enum MouseTrackingAxes axes;
    
    NSView *trackedView;
    NSRect trackedViewOldFrame;
    NSUInteger trackedViewResizingMask;
}

- (id) initWithContentRect: (NSRect) contentRect
                drawHelper: (void (^)(NSRect drawFrame)) newDrawHelper
               aboveWindow: (NSWindow *) aboveWindow;
- (void) remove;
- (void) startTrackingMouse: (NSPoint) newLocation
                     onAxes: (enum MouseTrackingAxes) newAxes;
- (void) updateMouse: (NSPoint) newLocation;
- (void) offsetBy: (NSSize) offsetAmount;
@end
