#import <Cocoa/Cocoa.h>
#import "SizeConstraintParticipant.h"


@interface DocumentContentView : NSView <SizeConstraintParticipant>
{
    NSScroller *verticalScroller;
    NSTextStorage *textStorage;
    NSLayoutManager *layoutManager;
    NSTextContainer *textContainer;
}

+ (CGFloat) leftMarginWidth;
+ (CGFloat) rightMarginWidth;
+ (CGFloat) rightPaddingWidth;
- (id) initWithFrame: (NSRect) frame;
- (void) resizeSubviewsWithOldSize: (NSSize) oldBoundsSize;
- (NSSize) desiredSize;
- (BOOL) isFlipped;
- (void) drawRect: (NSRect) dirtyRect;
- (IBAction) scrollerActivated: (id) sender;
- (void) mouseDown: (NSEvent *) event;
@end
