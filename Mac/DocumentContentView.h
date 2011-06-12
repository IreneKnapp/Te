#import <Cocoa/Cocoa.h>
#import "SizeConstraintParticipant.h"


@class TransparentHelperWindow;
@interface DocumentContentView : NSView <SizeConstraintParticipant>
{
    NSScroller *verticalScroller;
    NSTextStorage *textStorage;
    NSLayoutManager *layoutManager;
    NSTextContainer *textContainer;
    TransparentHelperWindow *resizingTip;
}

+ (CGFloat) leftMarginWidth;
+ (CGFloat) rightMarginWidth;
+ (CGFloat) rightPaddingWidth;
- (id) initWithFrame: (NSRect) frame;
- (void) resizeSubviewsWithOldSize: (NSSize) oldBoundsSize;
- (NSSize) desiredSize;
- (BOOL) isFlipped;
- (void) drawRect: (NSRect) dirtyRect;
- (void) showResizingTips;
- (void) hideResizingTips;
- (IBAction) scrollerActivated: (id) sender;
- (void) mouseDown: (NSEvent *) event;
@end
