#import "Window/Document/PaneManager.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "TransparentHelperWindow.h"
#import "Window/Document/PaneInformation.h"
#import "Utilities.h"

static WindowDocumentPaneManager *sharedManager = nil;


@implementation WindowDocumentPaneManager
@synthesize panesInformation;
@synthesize textStorage;
@synthesize layoutManager;
@synthesize textContainer;

+ (id) sharedManager {
    if(sharedManager == nil) {
        sharedManager = [[super allocWithZone: nil] init];
        
        sharedManager.panesInformation
            = [NSMutableArray arrayWithCapacity: 256];
        
        sharedManager.textStorage = [[NSTextStorage alloc] init];
        
        sharedManager.layoutManager = [[NSLayoutManager alloc] init];
        [sharedManager.textStorage
           addLayoutManager: sharedManager.layoutManager];
        
        NSSize containerSize = NSMakeSize(INFINITY, INFINITY);
        sharedManager.textContainer
            = [[NSTextContainer alloc] initWithContainerSize: containerSize];
        [sharedManager.textContainer setLineFragmentPadding: 0.0];
        [sharedManager.layoutManager
           addTextContainer: sharedManager.textContainer];
        
        NSNotificationCenter *notificationCenter
            = [NSNotificationCenter defaultCenter];
        
        [notificationCenter
          addObserver: self
          selector: @selector(preferredScrollerStyleDidChange:)
          name: NSPreferredScrollerStyleDidChangeNotification
          object: nil];
    }
    return sharedManager;
}


+ (id) allocWithZone: (NSZone *) zone {
    return [self sharedManager];
}


- (id) copyWithZone: (NSZone *) zone {
    return self;
}


- (void) addPane: (uuid_t *) documentPaneID
        toWindow: (NSWindow *) window
       withFrame: (NSRect) frame
{
    void *applicationState
        = [(AppDelegate *) [NSApp delegate] applicationState];
    if(!applicationState) {
        return;
    }
    
    NSScrollerStyle preferredScrollerStyle
        = [NSScroller preferredScrollerStyle];
    
    CGFloat scrollerWidth
        = [NSScroller scrollerWidthForControlSize: NSRegularControlSize
                      scrollerStyle: preferredScrollerStyle];
    CGFloat leftMarginWidth
        = teDocumentPaneLeftMarginWidth(applicationState, documentPaneID);
    
    NSRect verticalScrollerFrame;
    verticalScrollerFrame.size.width = scrollerWidth;
    verticalScrollerFrame.size.height = scrollerWidth * 10.0;
    verticalScrollerFrame.origin.x = 0.0;
    verticalScrollerFrame.origin.y = 0.0;
    NSScroller *verticalScroller
        = [[NSScroller alloc] initWithFrame: verticalScrollerFrame];
    verticalScrollerFrame.size.width = scrollerWidth;
    verticalScrollerFrame.size.height = frame.size.height - scrollerWidth;
    verticalScrollerFrame.origin.x = frame.size.width - scrollerWidth;
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
    [[window contentView] addSubview: verticalScroller];
    
    NSRect horizontalScrollerFrame;
    horizontalScrollerFrame.size.width = scrollerWidth * 10.0;
    horizontalScrollerFrame.size.height = scrollerWidth;
    horizontalScrollerFrame.origin.x = 0.0;
    horizontalScrollerFrame.origin.y = 0.0;
    NSScroller *horizontalScroller
        = [[NSScroller alloc] initWithFrame: horizontalScrollerFrame];
    horizontalScrollerFrame.size.width = frame.size.width - scrollerWidth;
    horizontalScrollerFrame.size.height = scrollerWidth;
    horizontalScrollerFrame.origin.x = 0.0;
    horizontalScrollerFrame.origin.y = frame.size.height - scrollerWidth;
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
    [[window contentView] addSubview: horizontalScroller];
    
    NSTimer *scrollerHidingTimer = nil;
    
    WindowDocumentPaneInformation *paneInformation
        = [[WindowDocumentPaneInformation alloc]
             initWithUUID: documentPaneID
             window: window
             frame: frame
             verticalScroller: verticalScroller
             horizontalScroller: horizontalScroller
             scrollerHidingTimer: scrollerHidingTimer];
    [panesInformation addObject: paneInformation];
}


- (void) repackScrollbars {
    void *applicationState
        = [(AppDelegate *) [NSApp delegate] applicationState];
    if(!applicationState) {
        return;
    }
    
    NSScrollerStyle preferredScrollerStyle
        = [NSScroller preferredScrollerStyle];
    CGFloat scrollerSize
        = [NSScroller scrollerWidthForControlSize: NSRegularControlSize
                      scrollerStyle: preferredScrollerStyle];
    
    for(WindowDocumentPaneInformation *paneInformation in panesInformation) {
        uuid_t *uuid = [paneInformation uuid];
        NSRect frame = [paneInformation frame];
        NSScroller *horizontalScroller = [paneInformation horizontalScroller];
        NSScroller *verticalScroller = [paneInformation verticalScroller];
        
        CGFloat leftMarginWidth
            = (CGFloat) teDocumentPaneLeftMarginWidth(applicationState, uuid);
        
        if(horizontalScroller) {
            NSRect horizontalScrollerFrame;
            horizontalScrollerFrame.size.width
                = frame.size.width - scrollerSize;
            horizontalScrollerFrame.size.height = scrollerSize;
            horizontalScrollerFrame.origin.x = 0.0;
            horizontalScrollerFrame.origin.y
                = frame.size.height - scrollerSize;
            if([NSScroller preferredScrollerStyle] != NSScrollerStyleLegacy) {
                horizontalScrollerFrame.origin.x += leftMarginWidth;
                horizontalScrollerFrame.size.width -= leftMarginWidth;
            }
            [horizontalScroller setFrame: horizontalScrollerFrame];
            [horizontalScroller setNeedsDisplay: YES];
        }
        
        if(verticalScroller) {
            NSRect verticalScrollerFrame;
            verticalScrollerFrame.size.height
                = frame.size.height - scrollerSize;
            verticalScrollerFrame.size.width = scrollerSize;
            verticalScrollerFrame.origin.x
                = frame.size.width - scrollerSize;
            verticalScrollerFrame.origin.y = 0.0;
            [verticalScroller setFrame: verticalScrollerFrame];
            [verticalScroller setNeedsDisplay: YES];
        }
    }
}


- (void) drawRect: (NSRect) dirtyRect
         ofWindow: (NSWindow *) window
{
    void *applicationState
        = [(AppDelegate *) [NSApp delegate] applicationState];
    if(!applicationState) {
        return;
    }
    
    NSFont *baseFont = [(AppDelegate *) [NSApp delegate] baseFont];
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    
    NSFont *lineNumberFont = [(AppDelegate *) [NSApp delegate] lineNumberFont];
    CGFloat lineNumberLineHeight
        = [(AppDelegate *) [NSApp delegate] lineNumberLineHeight];
    CGFloat lineNumberTopOffset = (lineHeight - lineNumberLineHeight) / 2.0;
    CGFloat lineNumberEmWidth
        = [(AppDelegate *) [NSApp delegate] lineNumberEmWidth];
    
    for(WindowDocumentPaneInformation *paneInformation in panesInformation) {
        NSWindow *paneWindow = [paneInformation window];
        if(![paneWindow isEqual: window]) {
            continue;
        }
        
        uuid_t *documentPaneID = [paneInformation uuid];
        NSRect frame = [paneInformation frame];
        
        CGFloat farLeft = 0.0;
        CGFloat farRight = frame.size.width;
        NSInteger leftMarginWidth
            = teDocumentPaneLeftMarginWidth(applicationState, documentPaneID);
        CGFloat contentAreaLeft = farLeft + leftMarginWidth;
        NSInteger leftPaddingWidth
            = teDocumentPaneLeftPaddingWidth(applicationState, documentPaneID);
        CGFloat textLeft = contentAreaLeft + leftPaddingWidth;
        CGFloat contentAreaRight
            = ceil(contentAreaLeft + leftPaddingWidth + emWidth * 80.0);
        
        CGFloat lineNumberPaddingWidth
            = teDocumentPaneLineNumberPaddingWidth
                (applicationState, documentPaneID);
        CGFloat leftMarginLineNumberAreaWidth
            = teDocumentPaneLineNumberAreaWidth
                (applicationState, documentPaneID);
        CGFloat leftMarginStatusAreaWidth
            = leftMarginWidth - leftMarginLineNumberAreaWidth;
        
        CGFloat totalHeight = frame.size.height;
        CGFloat contentAreaTop = 0.0;
        NSInteger bottomMarginWidth
            = teDocumentPaneBottomMarginWidth(applicationState, documentPaneID);
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
        NSColor *lineNumberColor
            = [NSColor colorWithDeviceWhite: 0.65 alpha: 1.0];
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
                = [NSString stringWithFormat: @"%4lu", i + 1];
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
}


- (IBAction) scrollerActivated: (id) sender {
    NSUInteger frameIndex = 0;
    for(WindowDocumentPaneInformation *paneInformation in panesInformation) {
        NSScroller *horizontalScroller = [paneInformation horizontalScroller];
        NSScroller *verticalScroller = [paneInformation verticalScroller];
        
        BOOL found = NO;
        if([sender isEqual: horizontalScroller]) {
            found = YES;
        } else if([sender isEqual: verticalScroller]) {
            found = YES;
        }
        
        if(found) {
            NSScrollerPart part = [sender hitPart];
            if(part == NSScrollerKnob) {
                [self showScrollersForFrame: frameIndex];
            } else if(part == NSScrollerKnobSlot) {
                [self hideScrollersAfterDelayForFrame: frameIndex];
            } else {
                [self flashScrollersForFrame: frameIndex];
            }
            
            break;
        }
        
        frameIndex++;
    }
}


- (void) showScrollersForFrame: (NSUInteger) frameIndex {
    if([NSScroller preferredScrollerStyle] != NSScrollerStyleOverlay)
        return;
    
    WindowDocumentPaneInformation *paneInformation
        = [panesInformation objectAtIndex: frameIndex];
    NSTimer *scrollerHidingTimer = [paneInformation scrollerHidingTimer];
    NSScroller *horizontalScroller = [paneInformation horizontalScroller];
    NSScroller *verticalScroller = [paneInformation verticalScroller];
    
    if(scrollerHidingTimer) {
        [scrollerHidingTimer invalidate];
        [paneInformation setScrollerHidingTimer: nil];
    }
    
    [horizontalScroller setAlphaValue: 1.0];
    [verticalScroller setAlphaValue: 1.0];
}


- (void) hideScrollersAfterDelayForFrame: (NSUInteger) frameIndex {
    if([NSScroller preferredScrollerStyle] != NSScrollerStyleOverlay)
        return;
    
    WindowDocumentPaneInformation *paneInformation
        = [panesInformation objectAtIndex: frameIndex];
    NSTimer *scrollerHidingTimer = [paneInformation scrollerHidingTimer];
    
    if(scrollerHidingTimer) {
        [scrollerHidingTimer invalidate];
        scrollerHidingTimer = nil;
    }
    
    scrollerHidingTimer
        = [NSTimer scheduledTimerWithTimeInterval: 1.5
                   target: self
                   selector: @selector(hideScrollersAfterDelayTimerFired:)
                   userInfo: paneInformation
                   repeats: NO];
    
    [paneInformation setScrollerHidingTimer: scrollerHidingTimer];
}


- (void) hideScrollersAfterDelayTimerFired: (NSTimer *) timer {
    if([NSScroller preferredScrollerStyle] != NSScrollerStyleOverlay)
        return;
    
    WindowDocumentPaneInformation *paneInformation = [timer userInfo];
    NSScroller *horizontalScroller = [paneInformation horizontalScroller];
    NSScroller *verticalScroller = [paneInformation verticalScroller];
    
    [paneInformation setScrollerHidingTimer: nil];
    [[horizontalScroller animator] setAlphaValue: 0.0];
    [[verticalScroller animator] setAlphaValue: 0.0];
}


- (void) flashScrollersForFrame: (NSUInteger) frameIndex {
    if([NSScroller preferredScrollerStyle] != NSScrollerStyleOverlay)
        return;
    
    [self showScrollersForFrame: frameIndex];
    [self hideScrollersAfterDelayForFrame: frameIndex];
}


- (void) preferredScrollerStyleDidChange: (NSNotification *) notification {
    NSScrollerStyle preferredScrollerStyle
        = [NSScroller preferredScrollerStyle];
    
    for(WindowDocumentPaneInformation *paneInformation in panesInformation) {
        NSTimer *scrollerHidingTimer = [paneInformation scrollerHidingTimer];
        NSScroller *horizontalScroller = [paneInformation horizontalScroller];
        NSScroller *verticalScroller = [paneInformation verticalScroller];
        
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
    }
    
    [self repackScrollbars];
}


- (void) mouseDown: (NSEvent *) event {
    NSLog(@"Click made it to pane manager.");
}

@end
