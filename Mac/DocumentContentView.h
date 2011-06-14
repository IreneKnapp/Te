#import <Cocoa/Cocoa.h>
#import "SizeConstraintParticipant.h"


@class TransparentHelperWindow;
@interface DocumentContentView : NSView <SizeConstraintParticipant>
{
    NSScroller *verticalScroller;
    NSScroller *horizontalScroller;
    NSTextStorage *textStorage;
    NSLayoutManager *layoutManager;
    NSTextContainer *textContainer;
    TransparentHelperWindow *resizingTip;
}

+ (CGFloat) leftMarginWidth;
+ (CGFloat) lineNumberPaddingWidth;
+ (CGFloat) rightPaddingWidth;
+ (NSUInteger) minimumLines;
+ (NSUInteger) minimumColumns;
+ (CGFloat) collapseLines;
+ (CGFloat) collapseColumns;
- (id) initWithFrame: (NSRect) frame;
- (void) resizeSubviewsWithOldSize: (NSSize) oldBoundsSize;
- (NSSize) desiredSize;
- (BOOL) isFlipped;
- (void) drawRect: (NSRect) dirtyRect;
- (void) showResizingTips;
- (void) hideResizingTips;
- (IBAction) scrollerActivated: (id) sender;
- (void) preferredScrollerStyleDidChange: (NSNotification *) notification;
- (void) mouseDown: (NSEvent *) event;
@end
