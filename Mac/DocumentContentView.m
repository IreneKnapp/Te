#import "DocumentContentView.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "TransparentHelperWindow.h"
#import "Utilities.h"


@implementation DocumentContentView

+ (CGFloat) leftMarginWidth {
    CGFloat lineNumberEmWidth
        = [(AppDelegate *) [NSApp delegate] lineNumberEmWidth];
    return 4.0 * lineNumberEmWidth + 16.0;
}


+ (CGFloat) rightMarginWidth {
    CGFloat scrollerWidth = [NSScroller scrollerWidth];
    return scrollerWidth;
}


+ (CGFloat) rightPaddingWidth {
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    return emWidth / 2.0;
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
        
        resizingTip = nil;
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


- (NSSize) desiredSize {
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    
    NSSize currentSize = [self frame].size;
    
    CGFloat leftMarginWidth = [DocumentContentView leftMarginWidth];
    CGFloat rightMarginWidth = [DocumentContentView rightMarginWidth];
    CGFloat contentWidth
        = currentSize.width - leftMarginWidth - rightMarginWidth;
    
    NSUInteger nLines = floor(currentSize.height / lineHeight);
    NSUInteger nColumns = floor(contentWidth / emWidth);
    
    NSSize result = NSMakeSize(nColumns * emWidth, nLines * lineHeight);
    result.width += [DocumentContentView leftMarginWidth];
    result.width += [DocumentContentView rightMarginWidth];
    result.width = ceil(result.width);
    return result;
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


- (void) showResizingTips {
    if(resizingTip)
        [self hideResizingTips];
    
    NSWindow *window = [self window];
    if(!window)
        return;
    
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    
    CGFloat horizontalPadding = emWidth * 0.5;
    CGFloat verticalPadding = lineHeight * 0.5;
    CGFloat border = 1.0;
    CGFloat rightMarginWidth = [DocumentContentView rightMarginWidth];
    
    NSRect contentRect;
    contentRect.size.width
        = emWidth * 9.0 + (horizontalPadding + border) * 2.0;
    contentRect.size.height
        = lineHeight + (verticalPadding + border) * 2.0;
    contentRect.origin.x
        = [self frame].size.width - contentRect.size.width - rightMarginWidth;
    contentRect.origin.y = [self frame].size.height - contentRect.size.height;
    contentRect = [self convertRectToBase: contentRect];
    
    void (^drawHelper)(NSRect frame)
        = ^(NSRect frame) {
            [[NSColor colorWithDeviceWhite: 1.0 alpha: 1.0] set];
            [NSBezierPath fillRect: frame];
            
            [[NSColor colorWithDeviceWhite: 0.5 alpha: 1.0] set];
            [NSBezierPath strokeRect: frame];
            
            [[NSColor redColor] set];
            [NSBezierPath fillRect: frame];
        };
    
    resizingTip = [[TransparentHelperWindow alloc]
                    initWithContentRect: contentRect
                    drawHelper: drawHelper
                    aboveWindow: window];
    [resizingTip setAlphaValue: 1.0];
    [resizingTip setOpaque: YES];
    [resizingTip startTrackingView: self
                 resizingMask: NSViewMinXMargin | NSViewMaxYMargin];
}


- (void) hideResizingTips {
    if(resizingTip) {
        [resizingTip remove];
        resizingTip = nil;
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
