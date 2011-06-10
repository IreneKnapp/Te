#import <Cocoa/Cocoa.h>


enum Axes {
  BothAxes,
  HorizontalAxis,
  VerticalAxis
};

@interface TransparentHelperWindow : NSWindow
{
    NSPoint savedMouseLocation;
    enum Axes axes;
}

- (id) initWithContentRect: (NSRect) contentRect
                drawHelper: (void (^)(NSRect drawFrame)) newDrawHelper
               aboveWindow: (NSWindow *) aboveWindow;
- (void) remove;
- (void) startTrackingMouse: (NSPoint) newLocation
                     onAxes: (enum Axes) newAxes;
- (void) updateMouse: (NSPoint) newLocation;
- (void) offsetBy: (NSSize) offsetAmount;
@end
