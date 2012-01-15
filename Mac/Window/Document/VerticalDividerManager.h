#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@interface VerticalDividerManager
{
}

+ (id) sharedManager;
+ (id) allocWithZone: (NSZone *) zone;
- (id) copyWithZone: (NSZone *) zone;
- (void) drawGhost: (NSRect) frame;
- (void) drawInFrame: (NSRect) dividerFrame
          isLeftmost: (BOOL) isBottom
         activeState: (BOOL) activeState
@end