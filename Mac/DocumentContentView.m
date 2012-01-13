#import "DocumentContentView.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "TransparentHelperWindow.h"
#import "Utilities.h"


@implementation DocumentContentView

+ (CGFloat) leftMarginWidth {
    CGFloat lineNumberAreaWidth = [DocumentContentView lineNumberAreaWidth];
    return lineNumberAreaWidth + 16.0;
}


+ (CGFloat) lineNumberPaddingWidth {
    return 2.0;
}


+ (CGFloat) lineNumberAreaWidth {
    CGFloat lineNumberPaddingWidth
        = [DocumentContentView lineNumberPaddingWidth];
    CGFloat lineNumberEmWidth
        = [(AppDelegate *) [NSApp delegate] lineNumberEmWidth];
    return lineNumberPaddingWidth + ceil(4.0 * lineNumberEmWidth);
}


+ (CGFloat) leftPaddingWidth {
    return 1.0;
}


+ (CGFloat) rightPaddingWidth {
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    return emWidth / 2.0;
}


+ (CGFloat) rightMarginWidth {
    NSScrollerStyle scrollerStyle = [NSScroller preferredScrollerStyle];
    if(scrollerStyle == NSScrollerStyleLegacy) {
        CGFloat scrollerWidth
            = [NSScroller scrollerWidthForControlSize: NSRegularControlSize
                          scrollerStyle: scrollerStyle];
        return scrollerWidth;
    } else {
        return 0.0;
    }
}


+ (CGFloat) bottomMarginWidth {
    NSScrollerStyle scrollerStyle = [NSScroller preferredScrollerStyle];
    if(scrollerStyle == NSScrollerStyleLegacy) {
        CGFloat scrollerWidth
            = [NSScroller scrollerWidthForControlSize: NSRegularControlSize
                          scrollerStyle: scrollerStyle];
        return scrollerWidth;
    } else {
        return 0.0;
    }
}


+ (NSUInteger) minimumLines {
    return 5;
}


+ (NSUInteger) minimumColumns {
    return 35;
}


+ (CGFloat) collapseLines {
    return 1.5;
}


+ (CGFloat) collapseColumns {
    return 17.5;
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
        if(preferredScrollerStyle == NSScrollerStyleOverlay) {
            [verticalScroller setWantsLayer: YES];
            [verticalScroller setAlphaValue: 0.0];
        }
        [self addSubview: verticalScroller];
        
        NSRect horizontalScrollerFrame;
        horizontalScrollerFrame.size.width = scrollerWidth * 10.0;
        horizontalScrollerFrame.size.height = scrollerWidth;
        horizontalScrollerFrame.origin.x = 0.0;
        horizontalScrollerFrame.origin.y = 0.0;
        horizontalScroller
            = [[NSScroller alloc] initWithFrame: horizontalScrollerFrame];
        horizontalScrollerFrame.size.width = bounds.size.width - scrollerWidth;
        horizontalScrollerFrame.size.height = scrollerWidth;
        horizontalScrollerFrame.origin.x = 0.0;
        horizontalScrollerFrame.origin.y = bounds.size.height - scrollerWidth;
        if([NSScroller preferredScrollerStyle] != NSScrollerStyleLegacy) {
            horizontalScrollerFrame.origin.x += leftMarginWidth;
            horizontalScrollerFrame.size.width -= leftMarginWidth;
        }
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
        if(preferredScrollerStyle == NSScrollerStyleOverlay) {
            [horizontalScroller setWantsLayer: YES];
            [horizontalScroller setAlphaValue: 0.0];
        }
        [self addSubview: horizontalScroller];
        
        scrollerHidingTimer = nil;
        
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
    }
    return self;
}


- (void) resizeSubviewsWithOldSize: (NSSize) oldBoundsSize {
    NSRect bounds = [self bounds];
    
    [self repackScrollbars];
}


- (void) repackScrollbars {
    CGFloat leftMarginWidth = [DocumentContentView leftMarginWidth];
    NSScrollerStyle preferredScrollerStyle
        = [NSScroller preferredScrollerStyle];
    CGFloat scrollerSize
        = [NSScroller scrollerWidthForControlSize: NSRegularControlSize
                      scrollerStyle: preferredScrollerStyle];
    NSRect bounds = [self bounds];
    
    if(horizontalScroller) {
        NSRect horizontalScrollerFrame;
        horizontalScrollerFrame.size.width = bounds.size.width - scrollerSize;
        horizontalScrollerFrame.size.height = scrollerSize;
        horizontalScrollerFrame.origin.x = 0.0;
        horizontalScrollerFrame.origin.y = bounds.size.height - scrollerSize;
        if([NSScroller preferredScrollerStyle] != NSScrollerStyleLegacy) {
            horizontalScrollerFrame.origin.x += leftMarginWidth;
            horizontalScrollerFrame.size.width -= leftMarginWidth;
        }
        [horizontalScroller setFrame: horizontalScrollerFrame];
        [horizontalScroller setNeedsDisplay: YES];
    }
    
    if(verticalScroller) {
        NSRect verticalScrollerFrame;
        verticalScrollerFrame.size.height = bounds.size.height - scrollerSize;
        verticalScrollerFrame.size.width = scrollerSize;
        verticalScrollerFrame.origin.x = bounds.size.width - scrollerSize;
        verticalScrollerFrame.origin.y = 0.0;
        [verticalScroller setFrame: verticalScrollerFrame];
        [verticalScroller setNeedsDisplay: YES];
    }
}


- (NSSize) minimumSize {
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    
    NSUInteger minimumLines = [DocumentContentView minimumLines];
    NSUInteger minimumColumns = [DocumentContentView minimumColumns];
    
    CGFloat width = [DocumentContentView leftMarginWidth]
                    + [DocumentContentView leftPaddingWidth]
                    + emWidth * minimumColumns
                    + [DocumentContentView rightPaddingWidth]
                    + [DocumentContentView rightMarginWidth];
    CGFloat height = lineHeight * minimumLines
                     + [DocumentContentView bottomMarginWidth];
    return NSMakeSize(width, height);
}


- (NSSize) desiredSize {
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    
    NSSize currentSize = [self frame].size;
    
    CGFloat leftMarginWidth = [DocumentContentView leftMarginWidth];
    CGFloat leftPaddingWidth = [DocumentContentView leftPaddingWidth];
    CGFloat rightPaddingWidth = [DocumentContentView rightPaddingWidth];
    CGFloat rightMarginWidth = [DocumentContentView rightMarginWidth];
    CGFloat contentWidth = currentSize.width - (leftMarginWidth
                                                + leftPaddingWidth
                                                + rightPaddingWidth
                                                + rightMarginWidth);
    
    CGFloat bottomMarginWidth = [DocumentContentView bottomMarginWidth];
    CGFloat contentHeight = currentSize.height - (bottomMarginWidth);
    
    NSUInteger nLines = 0;
    if(currentSize.height > 0.0)
        nLines = floor(contentHeight / lineHeight);
    NSUInteger nColumns = 0;
    if(contentWidth > 0.0)
        nColumns = floor(contentWidth / emWidth);
    
    NSUInteger minimumLines = [DocumentContentView minimumLines];
    if(nLines < minimumLines)
        nLines = minimumLines;
    
    NSUInteger minimumColumns = [DocumentContentView minimumColumns];
    if(nColumns < minimumColumns)
        nColumns = minimumColumns;
    
    NSSize result;
    result.width = nColumns * emWidth;
    result.width += leftMarginWidth;
    result.width += leftPaddingWidth;
    result.width += rightPaddingWidth;
    result.width += rightMarginWidth;
    result.width = ceil(result.width);
    result.height = nLines * lineHeight;
    result.height += bottomMarginWidth;
    result.height = ceil(result.height);
    return result;
}


- (NSString *) caption {
    return @"(12, 13) in 1980";
}


- (NSString *) sizeReport {
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    
    NSSize currentSize = [self frame].size;
    
    CGFloat leftMarginWidth = [DocumentContentView leftMarginWidth];
    CGFloat leftPaddingWidth = [DocumentContentView leftPaddingWidth];
    CGFloat rightPaddingWidth = [DocumentContentView rightPaddingWidth];
    CGFloat rightMarginWidth = [DocumentContentView rightMarginWidth];
    CGFloat contentWidth = currentSize.width - (leftMarginWidth
                                                + leftPaddingWidth
                                                + rightPaddingWidth
                                                + rightMarginWidth);
    
    CGFloat bottomMarginWidth = [DocumentContentView bottomMarginWidth];
    CGFloat contentHeight = currentSize.height - (bottomMarginWidth);
    
    NSUInteger nLines = 0;
    if(currentSize.height > 0.0)
        nLines = floor(contentHeight / lineHeight);
    NSUInteger nColumns = 0;
    if(contentWidth > 0.0)
        nColumns = floor(contentWidth / emWidth);
    
    return [NSString stringWithFormat: @"%lu x %lu",
                      (unsigned long) nColumns,
                      (unsigned long) nLines];
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
    CGFloat contentAreaLeft = farLeft + leftMarginWidth;
    CGFloat leftPaddingWidth = [DocumentContentView leftPaddingWidth];
    CGFloat textLeft = contentAreaLeft + leftPaddingWidth;
    CGFloat contentAreaRight
        = ceil(contentAreaLeft + leftPaddingWidth + emWidth * 80.0);
    
    CGFloat lineNumberPaddingWidth
        = [DocumentContentView lineNumberPaddingWidth];
    CGFloat leftMarginLineNumberAreaWidth
        = [DocumentContentView lineNumberAreaWidth];
    CGFloat leftMarginStatusAreaWidth
        = leftMarginWidth - leftMarginLineNumberAreaWidth;
    
    CGFloat totalHeight = [self bounds].size.height;
    CGFloat contentAreaTop = 0.0;
    CGFloat bottomMarginWidth = [DocumentContentView bottomMarginWidth];
    CGFloat contentAreaBottom = totalHeight - bottomMarginWidth;
    NSUInteger nLines
        = floor((contentAreaBottom - contentAreaTop) / lineHeight);
    
    NSRect contentArea
        = NSMakeRect(contentAreaLeft, contentAreaTop,
                     contentAreaRight - contentAreaLeft,
                     contentAreaBottom - contentAreaTop);
    NSRect leftMarginLineNumberArea
        = NSMakeRect(farLeft, contentAreaTop,
                     leftMarginLineNumberAreaWidth,
                     contentAreaBottom - contentAreaTop);
    NSRect leftMarginStatusArea
        = NSMakeRect(farLeft + leftMarginLineNumberAreaWidth, contentAreaTop,
                     leftMarginStatusAreaWidth,
                     contentAreaBottom - contentAreaTop);
    NSRect rightMarginArea
        = NSMakeRect(contentAreaRight, contentAreaTop,
                     farRight - contentAreaRight,
                     contentAreaBottom - contentAreaTop);
    NSRect bottomMarginArea
        = NSMakeRect(farLeft, contentAreaBottom,
                     farRight - farLeft, totalHeight - contentAreaBottom);
    
    [[NSColor whiteColor] set];
    [NSBezierPath fillRect: contentArea];
    [NSBezierPath fillRect: leftMarginLineNumberArea];
    [NSBezierPath fillRect: bottomMarginArea];
    
    [[NSColor colorWithDeviceWhite: 0.90 alpha: 1.0] set];
    [NSBezierPath fillRect: leftMarginStatusArea];
    [NSBezierPath fillRect: rightMarginArea];
    
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
        if(i == 0)
            contentString = @"1234567890123456789012345678901234567890"
                             "1234567890123456789012345678901234567890";
        if(i == 1)
            contentString = @"         1         2         3         4"
                             "         5         6         7         8";
        NSAttributedString *attributedString
            = [[NSAttributedString alloc] initWithString: contentString
                                          attributes: contentAttributes];
        [textStorage setAttributedString: attributedString];
        
        NSRange allGlyphs = NSMakeRange(0, [layoutManager numberOfGlyphs]);
        [layoutManager drawGlyphsForGlyphRange: allGlyphs
                       atPoint: NSMakePoint(textLeft, lineTop)];
        
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


- (IBAction) scrollerActivated: (id) sender {
    NSScrollerPart part = [sender hitPart];
    if(part == NSScrollerKnob) {
        [self showScrollers];
    } else if(part == NSScrollerKnobSlot) {
        [self hideScrollersAfterDelay];
    } else {
        [self flashScrollers];
    }
    
    if([sender isEqual: horizontalScroller]) {
    } else if([sender isEqual: verticalScroller]) {
    }
}


- (void) showScrollers {
    if([NSScroller preferredScrollerStyle] != NSScrollerStyleOverlay)
        return;
    
    if(scrollerHidingTimer) {
        [scrollerHidingTimer invalidate];
        scrollerHidingTimer = nil;
    }
    
    [horizontalScroller setAlphaValue: 1.0];
    [verticalScroller setAlphaValue: 1.0];
}


- (void) hideScrollersAfterDelay {
    if([NSScroller preferredScrollerStyle] != NSScrollerStyleOverlay)
        return;
    
    if(scrollerHidingTimer) {
        [scrollerHidingTimer invalidate];
        scrollerHidingTimer = nil;
    }
    
    scrollerHidingTimer
        = [NSTimer scheduledTimerWithTimeInterval: 1.5
                   target: self
                   selector: @selector(hideScrollersAfterDelayTimerFired:)
                   userInfo: nil
                   repeats: NO];
}


- (void) hideScrollersAfterDelayTimerFired: (NSTimer *) timer {
    if([NSScroller preferredScrollerStyle] != NSScrollerStyleOverlay)
        return;
    
    scrollerHidingTimer = nil;
    [[horizontalScroller animator] setAlphaValue: 0.0];
    [[verticalScroller animator] setAlphaValue: 0.0];
}


- (void) flashScrollers {
    if([NSScroller preferredScrollerStyle] != NSScrollerStyleOverlay)
        return;
    
    [self showScrollers];
    [self hideScrollersAfterDelay];
}


- (void) preferredScrollerStyleDidChange: (NSNotification *) notification {
    NSScrollerStyle preferredScrollerStyle
        = [NSScroller preferredScrollerStyle];
    
    if(scrollerHidingTimer) {
        [scrollerHidingTimer invalidate];
        scrollerHidingTimer = nil;
    }
    
    if(horizontalScroller) {
        [horizontalScroller setScrollerStyle: preferredScrollerStyle];
        if(preferredScrollerStyle == NSScrollerStyleOverlay) {
            [horizontalScroller setWantsLayer: YES];
            [horizontalScroller setAlphaValue: 0.0];
        } else {
            [horizontalScroller setAlphaValue: 1.0];
            [horizontalScroller setWantsLayer: NO];
        }
    }
    
    if(verticalScroller) {
        [verticalScroller setScrollerStyle: preferredScrollerStyle];
        if(preferredScrollerStyle == NSScrollerStyleOverlay) {
            [verticalScroller setWantsLayer: YES];
            [verticalScroller setAlphaValue: 0.0];
        } else {
            [verticalScroller setAlphaValue: 1.0];
            [verticalScroller setWantsLayer: NO];
        }
    }
    
    [self repackScrollbars];
}


- (void) mouseDown: (NSEvent *) event {
    NSLog(@"Click made it to content view.");
}

@end
