#import "DocumentContentView.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "Utilities.h"


@implementation DocumentContentView

+ (CGFloat) leftMarginWidth {
    return 20.0;
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
        verticalScrollerFrame.size.height = bounds.size.height;
        verticalScrollerFrame.origin.x = bounds.size.width - scrollerWidth;
        verticalScrollerFrame.origin.y = 0.0;
        verticalScroller
            = [[NSScroller alloc] initWithFrame: verticalScrollerFrame];
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


- (BOOL) isFlipped {
    return YES;
}


- (void) drawRect: (NSRect) dirtyRect {
    NSFont *baseFont = [(AppDelegate *) [NSApp delegate] baseFont];
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    
    CGFloat farLeft = 0.0;
    CGFloat leftMarginWidth = [DocumentContentView leftMarginWidth];
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
    NSRect leftMarginArea
        = NSMakeRect(farLeft, contentAreaTop,
                     leftMarginWidth, contentAreaHeight);
    NSRect rightMarginArea
        = NSMakeRect(contentAreaRight, contentAreaTop,
                     rightMarginWidth, contentAreaHeight);
    NSRect leftMarginLine
        = NSMakeRect(contentAreaLeft - 1.0, contentAreaTop,
                     1.0, contentAreaHeight);
    NSRect rightMarginLine
        = NSMakeRect(contentAreaRight, contentAreaTop,
                     1.0, contentAreaHeight);
    
    [[NSColor whiteColor] set];
    [NSBezierPath fillRect: contentArea];
    
    [[NSColor colorWithDeviceWhite: 0.90 alpha: 1.0] set];
    [NSBezierPath fillRect: leftMarginArea];
    [NSBezierPath fillRect: rightMarginArea];
    
    [[NSColor colorWithDeviceWhite: 0.85 alpha: 1.0] set];
    [NSBezierPath fillRect: leftMarginLine];
    [NSBezierPath fillRect: rightMarginLine];
    
    NSMutableDictionary *attributes
        = [NSMutableDictionary dictionaryWithCapacity: 1];
    [attributes setObject: baseFont forKey: NSFontAttributeName];
    
    CGFloat upcomingLineTop = 0.0;
    for(NSUInteger i = 0; i < nLines; i++) {
        CGFloat lineTop = upcomingLineTop;
        
        NSString *string =
            [NSString stringWithFormat:
                       @"%02i: Colorless green ideas sleep furiously.",
                       i];
        NSAttributedString *attributedString
            = [[NSAttributedString alloc] initWithString: string
                                          attributes: attributes];
        [textStorage setAttributedString: attributedString];
        
        NSRange allGlyphs = NSMakeRange(0, [layoutManager numberOfGlyphs]);
        [layoutManager drawGlyphsForGlyphRange: allGlyphs
                       atPoint: NSMakePoint(contentAreaLeft, lineTop)];
        
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
