#import <Cocoa/Cocoa.h>


@interface DocumentContentView : NSView
{
    NSScroller *verticalScroller;
    NSTextStorage *textStorage;
    NSLayoutManager *layoutManager;
    NSTextContainer *textContainer;
}

- (id) initWithFrame: (NSRect) frame;
- (BOOL) isFlipped;
- (void) drawRect: (NSRect) dirtyRect;
- (IBAction) scrollerActivated: (id) sender;
@end
