#import "DocumentContentView.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "TransparentHelperWindow.h"
#import "Utilities.h"


@implementation DocumentContentView

+ (CGFloat) leftMarginWidth {
    CGFloat lineNumberPaddingWidth
        = [DocumentContentView lineNumberPaddingWidth];
    CGFloat lineNumberEmWidth
        = [(AppDelegate *) [NSApp delegate] lineNumberEmWidth];
    return lineNumberPaddingWidth + 4.0 * lineNumberEmWidth + 16.0;
}


+ (CGFloat) lineNumberPaddingWidth {
    return 2.0;
}


+ (CGFloat) rightPaddingWidth {
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    return emWidth / 2.0;
}


+ (NSUInteger) minimumLines {
    return 5;
}


+ (NSUInteger) minimumColumns {
    return 25;
}


+ (CGFloat) collapseLines {
    return 1.5;
}


+ (CGFloat) collapseColumns {
    return 13.0;
}


- (id) initWithFrame: (NSRect) frame {
    self = [super initWithFrame: frame];
    if(self) {
        NSScrollerStyle preferredScrollerStyle
            = [NSScroller preferredScrollerStyle];
        
        CGFloat scrollerWidth
            = [NSScroller scrollerWidthForControlSize: NSRegularControlSize
                          scrollerStyle: preferredScrollerStyle];
        CGFloat leftMarginWidth = [DocumentContentView leftMarginWidth];
        
        NSRect bounds = [self bounds];
        
        NSRect verticalScrollerFrame;
        verticalScrollerFrame.size.width = scrollerWidth;
        verticalScrollerFrame.size.height = scrollerWidth * 10.0;
        verticalScrollerFrame.origin.x = 0.0;
        verticalScrollerFrame.origin.y = 0.0;
        verticalScroller
            = [[NSScroller alloc] initWithFrame: verticalScrollerFrame];
        verticalScrollerFrame.size.width = scrollerWidth;
        verticalScrollerFrame.size.height = bounds.size.height - scrollerWidth;
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
        [verticalScroller setScrollerStyle: preferredScrollerStyle];
        [verticalScroller setKnobStyle: NSScrollerKnobStyleDark];
        [self addSubview: verticalScroller];
        
        NSRect horizontalScrollerFrame;
        horizontalScrollerFrame.size.width = scrollerWidth * 10.0;
        horizontalScrollerFrame.size.height = scrollerWidth;
        horizontalScrollerFrame.origin.x = 0.0;
        horizontalScrollerFrame.origin.y = 0.0;
        horizontalScroller
            = [[NSScroller alloc] initWithFrame: horizontalScrollerFrame];
        horizontalScrollerFrame.size.width
            = bounds.size.width - scrollerWidth - leftMarginWidth;
        horizontalScrollerFrame.size.height = scrollerWidth;
        horizontalScrollerFrame.origin.x = leftMarginWidth;
        horizontalScrollerFrame.origin.y = bounds.size.height - scrollerWidth;
        [horizontalScroller setFrame: horizontalScrollerFrame];
        
        [horizontalScroller setAutoresizingMask:
                             NSViewMinYMargin | NSViewWidthSizable];
        [horizontalScroller setArrowsPosition: NSScrollerArrowsDefaultSetting];
        [horizontalScroller setKnobProportion: 0.75];
        [horizontalScroller setDoubleValue: 0.0];
        [horizontalScroller setTarget: self];
        [horizontalScroller setAction: @selector(scrollerActivated:)];
        [horizontalScroller setEnabled: YES];
        [horizontalScroller setScrollerStyle: preferredScrollerStyle];
        [horizontalScroller setKnobStyle: NSScrollerKnobStyleDark];
        [self addSubview: horizontalScroller];
        
        textStorage = [[NSTextStorage alloc] init];
        
        layoutManager = [[NSLayoutManager alloc] init];
        [textStorage addLayoutManager: layoutManager];
        
        NSSize containerSize = NSMakeSize(INFINITY, INFINITY);
        textContainer
            = [[NSTextContainer alloc] initWithContainerSize: containerSize];
        [textContainer setLineFragmentPadding: 0.0];
        [layoutManager addTextContainer: textContainer];
        
        NSNotificationCenter *notificationCenter
            = [NSNotificationCenter defaultCenter];
        
        [notificationCenter
          addObserver: self
          selector: @selector(preferredScrollerStyleDidChange:)
          name: NSPreferredScrollerStyleDidChangeNotification
          object: nil];
        
        resizingTip = nil;
    }
    return self;
}


- (void) resizeSubviewsWithOldSize: (NSSize) oldBoundsSize {
    NSRect bounds = [self bounds];
    
    CGFloat leftMarginWidth = [DocumentContentView leftMarginWidth];
    
    if(horizontalScroller) {
        NSRect horizontalScrollerFrame = [horizontalScroller frame];
        CGFloat scrollerHeight = horizontalScrollerFrame.size.height;
        horizontalScrollerFrame.size.width
            = bounds.size.width - scrollerHeight - leftMarginWidth;
        horizontalScrollerFrame.origin.x = leftMarginWidth;
        horizontalScrollerFrame.origin.y = bounds.size.height - scrollerHeight;
        [horizontalScroller setFrame: horizontalScrollerFrame];
        [horizontalScroller setNeedsDisplay: YES];
    }
    
    if(verticalScroller) {
        NSRect verticalScrollerFrame = [verticalScroller frame];
        CGFloat scrollerWidth = verticalScrollerFrame.size.width;
        verticalScrollerFrame.size.height = bounds.size.height - scrollerWidth;
        verticalScrollerFrame.origin.x = bounds.size.width - scrollerWidth;
        verticalScrollerFrame.origin.y = 0.0;
        [verticalScroller setFrame: verticalScrollerFrame];
        [verticalScroller setNeedsDisplay: YES];
    }
}


- (NSSize) desiredSize {
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    
    NSSize currentSize = [self frame].size;
    
    CGFloat leftMarginWidth = [DocumentContentView leftMarginWidth];
    CGFloat rightPaddingWidth = [DocumentContentView rightPaddingWidth];
    CGFloat contentWidth
        = currentSize.width - leftMarginWidth - rightPaddingWidth;
    
    NSUInteger nLines = 0;
    if(currentSize.height > 0.0)
        nLines = floor(currentSize.height / lineHeight);
    NSUInteger nColumns = 0;
    if(contentWidth > 0.0)
        nColumns = floor(contentWidth / emWidth);
    
    NSUInteger minimumLines = [DocumentContentView minimumLines];
    if(nLines < minimumLines)
        nLines = minimumLines;
    
    NSUInteger minimumColumns = [DocumentContentView minimumColumns];
    if(nColumns < minimumColumns)
        nColumns = minimumColumns;
    
    NSSize result = NSMakeSize(nColumns * emWidth, nLines * lineHeight);
    result.width += leftMarginWidth + rightPaddingWidth;
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
    CGFloat farRight = [self bounds].size.width;
    CGFloat leftMarginWidth = [DocumentContentView leftMarginWidth];
    CGFloat lineNumberPaddingWidth
        = [DocumentContentView lineNumberPaddingWidth];
    CGFloat leftMarginLineNumberAreaWidth
        = ceil(lineNumberPaddingWidth + 4.0 * lineNumberEmWidth);
    CGFloat leftMarginStatusAreaWidth
        = leftMarginWidth - leftMarginLineNumberAreaWidth;
    CGFloat contentAreaLeft = farLeft + leftMarginWidth;
    CGFloat contentAreaRight = farRight;
    CGFloat contentAreaWidth = contentAreaRight - contentAreaLeft;
    
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
    NSRect rightMarginBorder
        = NSMakeRect(emWidth * 80.0, contentAreaTop,
                     1.0, contentAreaHeight);
    
    [[NSColor whiteColor] set];
    [NSBezierPath fillRect: contentArea];
    
    [[NSColor colorWithDeviceWhite: 0.90 alpha: 1.0] set];
    [NSBezierPath fillRect: leftMarginStatusArea];
    [NSBezierPath fillRect: rightMarginBorder];
    
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
    
    CGFloat upcomingLineTop = 0.0;
    for(NSUInteger i = 0; i < nLines; i++) {
        CGFloat lineTop = upcomingLineTop;
        
        NSString *contentString = @"Colorless green ideas sleep furiously.";
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
                       atPoint: NSMakePoint(farLeft + lineNumberPaddingWidth,
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
    CGFloat scrollerWidth = [horizontalScroller frame].size.width;
    CGFloat scrollerHeight = [horizontalScroller frame].size.height;
    
    CGFloat horizontalPadding = emWidth * 0.5;
    CGFloat verticalPadding = lineHeight * 0.5;
    CGFloat border = 1.0;
    
    NSRect contentRect;
    contentRect.size.width
        = emWidth * 9.0 + (horizontalPadding + border) * 2.0;
    contentRect.size.height
        = lineHeight + (verticalPadding + border) * 2.0;
    contentRect.origin.x
        = [self frame].size.width - contentRect.size.width - scrollerWidth;
    contentRect.origin.y
        = [self frame].size.height - contentRect.size.height - scrollerHeight;
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
    if([sender isEqual: horizontalScroller]) {
    } else if([sender isEqual: verticalScroller]) {
    }
}


- (void) preferredScrollerStyleDidChange: (NSNotification *) notification {
    NSScrollerStyle preferredScrollerStyle
        = [NSScroller preferredScrollerStyle];
    
    if(horizontalScroller) {
        [horizontalScroller setScrollerStyle: preferredScrollerStyle];
        
        CGFloat newHeight
            = [NSScroller scrollerWidthForControlSize: NSRegularControlSize
                          scrollerStyle: preferredScrollerStyle];
        
        NSRect frame = [horizontalScroller frame];
        CGFloat heightDifference = newHeight - frame.size.height;
        frame.origin.y -= heightDifference;
        frame.size.height += heightDifference;
        [horizontalScroller setFrame: frame];
    }
    
    if(verticalScroller) {
        [verticalScroller setScrollerStyle: preferredScrollerStyle];
        
        CGFloat newWidth
            = [NSScroller scrollerWidthForControlSize: NSRegularControlSize
                          scrollerStyle: preferredScrollerStyle];
        
        NSRect frame = [verticalScroller frame];
        CGFloat widthDifference = newWidth - frame.size.width;
        frame.origin.x -= widthDifference;
        frame.size.width += widthDifference;
        [verticalScroller setFrame: frame];
    }
}


- (void) mouseDown: (NSEvent *) event {
    NSLog(@"Click made it to content view.");
}

@end
