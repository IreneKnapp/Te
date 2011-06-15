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
    NSTimer *scrollerHidingTimer;
}

+ (CGFloat) leftMarginWidth;
+ (CGFloat) lineNumberPaddingWidth;
+ (CGFloat) lineNumberAreaWidth;
+ (CGFloat) leftPaddingWidth;
+ (CGFloat) rightPaddingWidth;
+ (CGFloat) rightMarginWidth;
+ (CGFloat) bottomMarginWidth;
+ (NSUInteger) minimumLines;
+ (NSUInteger) minimumColumns;
+ (CGFloat) collapseLines;
+ (CGFloat) collapseColumns;
- (id) initWithFrame: (NSRect) frame;
- (void) resizeSubviewsWithOldSize: (NSSize) oldBoundsSize;
- (void) repackScrollbars;
- (NSSize) minimumSize;
- (NSSize) desiredSize;
- (NSString *) caption;
- (NSString *) sizeReport;
- (BOOL) isFlipped;
- (void) drawRect: (NSRect) dirtyRect;
- (IBAction) scrollerActivated: (id) sender;
- (void) showScrollers;
- (void) hideScrollersAfterDelay;
- (void) hideScrollersAfterDelayTimerFired: (NSTimer *) timer;
- (void) flashScrollers;
- (void) preferredScrollerStyleDidChange: (NSNotification *) notification;
- (void) mouseDown: (NSEvent *) event;
@end
