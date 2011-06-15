#import <Cocoa/Cocoa.h>

@protocol SizeConstraintParticipant
- (NSSize) minimumSize;
- (NSSize) desiredSize;
- (NSString *) caption;
- (NSString *) sizeReport;
@end
