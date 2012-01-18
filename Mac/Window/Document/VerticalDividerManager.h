#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@interface WindowDocumentVerticalDividerManager : NSObject
{
}

+ (id) sharedManager;
+ (id) allocWithZone: (NSZone *) zone;
- (id) copyWithZone: (NSZone *) zone;
- (void) drawGhost: (NSRect) frame;
- (void) drawInFrame: (NSRect) dividerFrame
          isLeftmost: (BOOL) isLeftmost
         activeState: (BOOL) activeState;
@end