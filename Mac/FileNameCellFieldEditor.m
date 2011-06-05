#import "FileNameCellFieldEditor.h"

@implementation FileNameCellFieldEditor

- (id) initWithFrame: (NSRect) frameRect
       textContainer: (NSTextContainer *) textContainer
{
    self = [super initWithFrame: frameRect textContainer: textContainer];
    if(self) {
        previousFrameValid = NO;
    }
    return self;
}


- (void) viewDidMoveToSuperview {
    previousFrameValid = NO;
}


- (BOOL) isOpaque {
    return NO;
}


- (void) drawRect: (NSRect) dirtyRect {
    if(previousFrameValid) {
        NSView *superview = [self superview];
        
        NSRect rectToRedisplay = previousFrame;
        CGFloat outset = 3.0;
        rectToRedisplay.origin.x -= outset;
        rectToRedisplay.size.width += outset * 2.0;
        rectToRedisplay.origin.y -= outset;
        rectToRedisplay.size.height += outset * 2.0;
        
        [superview setNeedsDisplayInRect: rectToRedisplay];
    }
    
    [NSGraphicsContext saveGraphicsState];
    
    NSSetFocusRingStyle(NSFocusRingAbove);
    
    [[NSColor whiteColor] set];
    NSRect bounds = [self bounds];
    [NSBezierPath fillRect: bounds];
    
    [NSGraphicsContext restoreGraphicsState];
    
    previousFrame = [self frame];
    previousFrameValid = YES;
    
    NSLayoutManager *layoutManager = [self layoutManager];
    NSUInteger numberOfGlyphs = [layoutManager numberOfGlyphs];
    if(numberOfGlyphs > 0) {
        NSRange allGlyphs = NSMakeRange(0, numberOfGlyphs);
        
        NSPoint origin = [self textContainerOrigin];
        [layoutManager drawBackgroundForGlyphRange: allGlyphs atPoint: origin];
        [layoutManager drawGlyphsForGlyphRange: allGlyphs atPoint: origin];
    }
}

@end
