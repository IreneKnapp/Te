#import "DocumentContentView.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "Utilities.h"


@implementation DocumentContentView

+ (CGFloat) leftMarginWidth {
    CGFloat lineNumberEmWidth
        = [(AppDelegate *) [NSApp delegate] lineNumberEmWidth];
    return 4.0 * lineNumberEmWidth + 16.0;
}


+ (CGFloat) rightMarginWidth {
    return 40.0 + [NSScroller scrollerWidth];
}


- (id) initWithFrame: (NSRect) frame {
    self = [super initWithFrame: frame];
    if(self) {
        CGFloat scrollerWidth = [NSScroller scrollerWidth];
        
        NSRect bounds = [self bounds];
        
        NSRect verticalScrollerFrame;
        verticalScrollerFrame.size.width = scrollerWidth;
        verticalScrollerFrame.size.height = scrollerWidth * 10.0;
        verticalScrollerFrame.origin.x = 0.0;
        verticalScrollerFrame.origin.y = 0.0;
        verticalScroller
            = [[NSScroller alloc] initWithFrame: verticalScrollerFrame];
        verticalScrollerFrame.size.width = scrollerWidth;
        verticalScrollerFrame.size.height = bounds.size.height;
        verticalScrollerFrame.origin.x = bounds.size.width - scrollerWidth;
        verticalScrollerFrame.origin.y = 0.0;
        [verticalScroller setFrame: verticalScrollerFrame];
        
        [verticalScroller setAutoresizingMask:
                           NSViewMinXMargin | NSViewHeightSizable];
        [verticalScroller setArrowsPosition: NSScrollerArrowsDefaultSetting];
        [verticalScroller setKnobProportion: 0.05];
        [verticalScroller setDoubleValue: 1.0];
        [verticalScroller setTarget: self];
        [verticalScroller setAction: @selector(scrollerActivated:)];
        [verticalScroller setEnabled: YES];
        [self addSubview: verticalScroller];
        
        textStorage = [[NSTextStorage alloc] init];
        
        layoutManager = [[NSLayoutManager alloc] init];
        [textStorage addLayoutManager: layoutManager];
        
        NSSize containerSize = NSMakeSize(INFINITY, INFINITY);
        textContainer
            = [[NSTextContainer alloc] initWithContainerSize: containerSize];
        [layoutManager addTextContainer: textContainer];
    }
    return self;
}


- (void) resizeSubviewsWithOldSize: (NSSize) oldBoundsSize {
    NSRect bounds = [self bounds];
    
    if(bounds.size.height > 0.0) {
        NSView *superview = [self superview];
        NSWindow *window = [self window];
        NSView *contentView = nil;
        if(window)
            contentView = [window contentView];
        
        if(superview && ![superview isEqual: contentView]) {
            CGFloat scrollerWidth = [NSScroller scrollerWidth];
            
            NSRect verticalScrollerFrame;
            verticalScrollerFrame.size.width = scrollerWidth;
            verticalScrollerFrame.size.height = bounds.size.height;
            verticalScrollerFrame.origin.x = bounds.size.width - scrollerWidth;
            verticalScrollerFrame.origin.y = 0.0;
            [verticalScroller setFrame: verticalScrollerFrame];
        }
    }
}


- (BOOL) isFlipped {
    return YES;
}


- (void) drawRect: (NSRect) dirtyRect {
    NSFont *baseFont = [(AppDelegate *) [NSApp delegate] baseFont];
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    
    NSFont *lineNumberFont = [(AppDelegate *) [NSApp delegate] lineNumberFont];
    CGFloat lineNumberLineHeight
        = [(AppDelegate *) [NSApp delegate] lineNumberLineHeight];
    CGFloat lineNumberTopOffset = (lineHeight - lineNumberLineHeight) / 2.0;
    CGFloat lineNumberEmWidth
        = [(AppDelegate *) [NSApp delegate] lineNumberEmWidth];
    
    CGFloat farLeft = 0.0;
    CGFloat leftMarginWidth = [DocumentContentView leftMarginWidth];
    CGFloat leftMarginLineNumberAreaWidth = 4.0 * lineNumberEmWidth;
    CGFloat leftMarginStatusAreaWidth
        = leftMarginWidth - leftMarginLineNumberAreaWidth;
    CGFloat contentAreaLeft = farLeft + leftMarginWidth;
    CGFloat contentAreaWidth = ceil(emWidth * 80.0);
    CGFloat contentAreaRight = contentAreaLeft + contentAreaWidth;
    CGFloat rightMarginWidth = [DocumentContentView rightMarginWidth];
    CGFloat farRight = contentAreaRight + rightMarginWidth;
    
    CGFloat totalHeight = [self bounds].size.height;
    NSUInteger nLines = floor(totalHeight / lineHeight);
    CGFloat contentAreaTop = 0.0;
    CGFloat contentAreaBottom = totalHeight;
    CGFloat contentAreaHeight = contentAreaBottom - contentAreaTop;
    
    NSRect contentArea
        = NSMakeRect(contentAreaLeft, contentAreaTop,
                     contentAreaWidth, contentAreaHeight);
    NSRect leftMarginLineNumberArea
        = NSMakeRect(farLeft, contentAreaTop,
                     leftMarginLineNumberAreaWidth, contentAreaHeight);
    NSRect leftMarginStatusArea
        = NSMakeRect(farLeft + leftMarginLineNumberAreaWidth, contentAreaTop,
                     leftMarginStatusAreaWidth, contentAreaHeight);
    NSRect rightMarginArea
        = NSMakeRect(contentAreaRight, contentAreaTop,
                     rightMarginWidth, contentAreaHeight);
    
    [[NSColor whiteColor] set];
    [NSBezierPath fillRect: contentArea];
    
    [[NSColor colorWithDeviceWhite: 0.90 alpha: 1.0] set];
    [NSBezierPath fillRect: leftMarginStatusArea];
    [NSBezierPath fillRect: rightMarginArea];
    
    [[NSColor colorWithDeviceWhite: 1.00 alpha: 1.0] set];
    [NSBezierPath fillRect: leftMarginLineNumberArea];
    
    NSMutableDictionary *contentAttributes
        = [NSMutableDictionary dictionaryWithCapacity: 1];
    [contentAttributes setObject: baseFont forKey: NSFontAttributeName];
    
    NSMutableDictionary *lineNumberAttributes
        = [NSMutableDictionary dictionaryWithCapacity: 2];
    [lineNumberAttributes setObject: lineNumberFont
                          forKey: NSFontAttributeName];
    NSColor *lineNumberColor = [NSColor colorWithDeviceWhite: 0.65 alpha: 1.0];
    [lineNumberAttributes setObject: lineNumberColor
                          forKey: NSForegroundColorAttributeName];
    NSMutableParagraphStyle *lineNumberParagraphStyle
        = [[NSParagraphStyle defaultParagraphStyle] mutableCopyWithZone: nil];
    [lineNumberParagraphStyle setAlignment: NSRightTextAlignment];
    [lineNumberAttributes setObject: lineNumberParagraphStyle
                          forKey: NSParagraphStyleAttributeName];
    
    CGFloat upcomingLineTop = 0.0;
    for(NSUInteger i = 0; i < nLines; i++) {
        CGFloat lineTop = upcomingLineTop;
        
        NSString *contentString =
            [NSString stringWithFormat:
                       @"Colorless green ideas sleep furiously.",
                       i];
        NSAttributedString *attributedString
            = [[NSAttributedString alloc] initWithString: contentString
                                          attributes: contentAttributes];
        [textStorage setAttributedString: attributedString];
        
        NSRange allGlyphs = NSMakeRange(0, [layoutManager numberOfGlyphs]);
        [layoutManager drawGlyphsForGlyphRange: allGlyphs
                       atPoint: NSMakePoint(contentAreaLeft, lineTop)];
        
        NSString *lineNumberString
            = [NSString stringWithFormat: @"%4u", i + 1];
        attributedString
            = [[NSAttributedString alloc] initWithString: lineNumberString
                                          attributes: lineNumberAttributes];
        [textStorage setAttributedString: attributedString];
        
        allGlyphs = NSMakeRange(0, [layoutManager numberOfGlyphs]);
        [layoutManager drawGlyphsForGlyphRange: allGlyphs
                       atPoint: NSMakePoint(farLeft,
                                            lineNumberTopOffset + lineTop)];
        
        upcomingLineTop = lineTop + lineHeight;
    }
}


- (IBAction) scrollerActivated: (id) sender {
    if([sender isEqual: verticalScroller]) {
    }
}


- (void) mouseDown: (NSEvent *) event {
    NSLog(@"Click made it to content view.");
}

@end
