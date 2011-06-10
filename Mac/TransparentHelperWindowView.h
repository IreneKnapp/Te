#import <Cocoa/Cocoa.h>


@interface TransparentHelperWindowView : NSView
{
    void (^drawHelper)(NSRect drawFrame);
}

- (id) initWithFrame: (NSRect) frame
          drawHelper: (void (^)(NSRect drawFrame)) newDrawHelper;
- (void) drawRect: (NSRect) dirtyRect;
- (NSView *) hitTest: (NSPoint) point;
@end
