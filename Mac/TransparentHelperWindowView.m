#import "TransparentHelperWindowView.h"


@implementation TransparentHelperWindowView

- (id) initWithFrame: (NSRect) frame
          drawHelper: (void (^)(NSRect drawFrame)) newDrawHelper;
{
    self = [super initWithFrame: frame];
    if(self) {
        drawHelper = newDrawHelper;
    }
    return self;
}


- (void) drawRect: (NSRect) dirtyRect {
    drawHelper([self bounds]);
}


- (NSView *) hitTest: (NSPoint) point {
    return nil;
}


- (BOOL) isFlipped {
    return YES;
}

@end
