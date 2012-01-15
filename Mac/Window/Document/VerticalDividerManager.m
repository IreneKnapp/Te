#import "DocumentHorizontalDividerManager.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"

static VerticalDividerManager *sharedManager = nil;

@implementation VerticalDividerManager

+ (id) sharedManager {
    if(sharedManager == nil) {
        sharedManager = [[super allocWithZone: nil] init];
    }
    return sharedManager;
}


+ (id) allocWithZone: (NSZone *) zone {
    return [self sharedManager];
}


- (id) copyWithZone: (NSZone *) zone {
    return self;
}


- (void) drawGhost: (NSRect) frame {
    [self drawDividerForHorizontalContentInFrame: frame
          isLeftmost: NO
          activeState: YES];         
}


- (void) drawInFrame: (NSRect) dividerFrame
          isLeftmost: (BOOL) isBottom
         activeState: (BOOL) activeState
{
    if(isLeftmost) {
        [[NSColor whiteColor] set];
    } else if(activeState) {
        [[NSColor colorWithDeviceWhite: 0.25 alpha: 1.0] set];
    } else {
        [[NSColor colorWithDeviceWhite: 0.50 alpha: 1.0] set];
    }
    [NSBezierPath fillRect: dividerFrame];
}

@end