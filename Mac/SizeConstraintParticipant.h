#import <Cocoa/Cocoa.h>

@protocol SizeConstraintParticipant
- (NSSize) desiredSize;
- (void) showResizingTips;
- (void) hideResizingTips;
@end
