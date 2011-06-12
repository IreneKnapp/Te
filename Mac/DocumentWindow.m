#import "DocumentWindow.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "DocumentContentView.h"
#import "DocumentSplitView.h"
#import "Utilities.h"


@implementation DocumentWindow

- (id) initWithWindowID: (uuid_t *) newWindowID {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return nil;
    
    NSUInteger styleMask = NSTitledWindowMask
                           | NSClosableWindowMask
                           | NSMiniaturizableWindowMask
                           | NSResizableWindowMask;
    
    NSSize initialSize = [self defaultSize];
    
    NSRect visibleFrame = [[NSScreen mainScreen] visibleFrame];
    
    NSRect contentRect;
    contentRect.size = initialSize;
    contentRect.origin.x
        = (visibleFrame.size.width - contentRect.size.width) / 2.0
          + visibleFrame.origin.x;
    contentRect.origin.y
        = visibleFrame.size.height - (contentRect.size.height + 22.0)
          + visibleFrame.origin.y;
    
    NSWindow *window = [[NSWindow alloc] initWithContentRect: contentRect
                                         styleMask: styleMask
                                         backing: NSBackingStoreBuffered
                                         defer: YES];
    
    self = [super initWithWindowID: newWindowID window: window];
    if(self) {
        [window setDelegate: self];
        
        adjustingSize = NO;
        stillLoading = YES;
        
        NSRect contentFrame = [[window contentView] bounds];
        
        documentSplitView
            = [[DocumentSplitView alloc] initWithFrame: contentFrame];
        [documentSplitView setAutoresizingMask:
                            NSViewWidthSizable | NSViewHeightSizable];
        [[window contentView] addSubview: documentSplitView];
        
        [documentSplitView newContentSubviewAtIndex: 0
                           alongAxis: UncommittedSplitAxis];
        
        [documentSplitView adjustSubviews];
        
        [self setConstraints];
        
        stillLoading = NO;
        
        manuallyAdjustedSize = initialSize;
        [self adjustSize: initialSize];
        
        [window orderFront: self];
    }
    return self;
}


- (NSSize) defaultSize {
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    
    CGFloat dividerHeight
        = [DocumentSplitView minimumDividerThicknessForAxis:
                              VerticalSplitAxis];
    CGFloat dividerWidth
        = [DocumentSplitView minimumDividerThicknessForAxis:
                              HorizontalSplitAxis];
    CGFloat width
        = dividerWidth
          + [DocumentContentView leftMarginWidth]
          + emWidth * 80.0
          + [DocumentContentView rightMarginWidth];
    CGFloat height = 50.0 * lineHeight + dividerHeight;
    
    NSSize visibleSize = [[NSScreen mainScreen] visibleFrame].size;
    
    NSInteger linesToRemove = 0;
    if(height > visibleSize.height)
        linesToRemove = ceil((height - visibleSize.height) / lineHeight);
    height -= linesToRemove * lineHeight;
    
    NSInteger columnsToRemove = 0;
    if(width > visibleSize.width)
        columnsToRemove = ceil((width - visibleSize.width) / emWidth);
    width -= columnsToRemove * emWidth;
    
    return NSMakeSize(width, height);
}


- (void) setConstraints {
    NSWindow *window = [self window];
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    CGFloat dividerHeight
        = [DocumentSplitView minimumDividerThicknessForAxis:
                              VerticalSplitAxis];
    CGFloat dividerWidth
        = [DocumentSplitView minimumDividerThicknessForAxis:
                              HorizontalSplitAxis];
    
    CGFloat minWidth
        = dividerWidth
          + [DocumentContentView leftMarginWidth]
          + emWidth * 40
          + [DocumentContentView rightMarginWidth];
    CGFloat minHeight = 10.0 * lineHeight + dividerHeight;
    
    [window setContentMinSize: NSMakeSize(minWidth, minHeight)];
    [window setContentMaxSize: NSMakeSize(INFINITY, INFINITY)];
    [window setContentResizeIncrements: NSMakeSize(emWidth, lineHeight)];
}


- (void) adjustSize: (NSSize) newSize {
    BOOL oldAdjustingSize = adjustingSize;
    adjustingSize = YES;
    
    NSWindow *window = [self window];
    NSRect frame = [window frame];
    NSSize contentSize = [window contentRectForFrameRect: frame].size;
    NSSize decorationSize = NSMakeSize(frame.size.width - contentSize.width,
                                       frame.size.height - contentSize.height);
    
    CGFloat widthDifference = newSize.width - contentSize.width;
    frame.origin.x -= floor(widthDifference / 2.0);
    
    CGFloat heightDifference = newSize.height - contentSize.height;
    frame.origin.y -= heightDifference;
    
    frame.size.width = newSize.width + decorationSize.width;
    frame.size.height = newSize.height + decorationSize.height;
    
    [window setFrame: frame display: NO];
    
    adjustingSize = oldAdjustingSize;
}


- (void) adjustSizePerContentConstraints {
    if(!adjustingSize && !stillLoading) {
        NSSize desiredSize = [documentSplitView desiredSize];
        
        CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
        CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
        
        NSInteger linesToAdd
            = floor((manuallyAdjustedSize.height - desiredSize.height)
                    / lineHeight);
        desiredSize.height += linesToAdd * lineHeight;
        
        NSInteger columnsToAdd
            = floor((manuallyAdjustedSize.width - desiredSize.width)
                    / emWidth);
        desiredSize.width += columnsToAdd * emWidth;
        
        [self adjustSize: desiredSize];
    }
}


- (void) windowWillStartLiveResize: (NSNotification *) notification {
    if(!stillLoading) {
        [documentSplitView showResizingTips];
        adjustingSize = YES;
    }
}


- (void) windowDidEndLiveResize: (NSNotification *) notification {
    if(!stillLoading) {
        [documentSplitView hideResizingTips];
        manuallyAdjustedSize = [[self window] frame].size;
    }
    adjustingSize = NO;
}


- (BOOL) getCurrentFolderInodeID: (uuid_t *) result {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return NO;
    
    // TODO
    return NO;
}

@end
