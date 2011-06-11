#import <Cocoa/Cocoa.h>


@interface DocumentContentView : NSView
{
    NSScroller *verticalScroller;
    NSTextStorage *textStorage;
    NSLayoutManager *layoutManager;
    NSTextContainer *textContainer;
}

+ (CGFloat) leftMarginWidth;
+ (CGFloat) rightMarginWidth;
- (id) initWithFrame: (NSRect) frame;
- (void) resizeSubviewsWithOldSize: (NSSize) oldBoundsSize;
- (BOOL) isFlipped;
- (void) drawRect: (NSRect) dirtyRect;
- (IBAction) scrollerActivated: (id) sender;
- (void) mouseDown: (NSEvent *) event;
@end
