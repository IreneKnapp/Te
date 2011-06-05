#import <Cocoa/Cocoa.h>

@interface FileNameCellFieldEditor : NSTextView
{
    BOOL previousFrameValid;
    NSRect previousFrame;
}

- (id) initWithFrame: (NSRect) frameRect
       textContainer: (NSTextContainer *) textContainer;
- (void) viewDidMoveToSuperview;
- (BOOL) isOpaque;
- (void) drawRect: (NSRect) dirtyRect;
@end
