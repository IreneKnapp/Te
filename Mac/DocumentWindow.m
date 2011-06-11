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
    
    self = [super initWithWindowID: newWindowID nibName: @"DocumentWindow"];
    if(self) {
        NSWindow *window = [self window];
        
        [self sizeToDefault];
        [self setConstraints];
        
        NSRect contentFrame = [[window contentView] bounds];
        
        documentSplitView
            = [[DocumentSplitView alloc] initWithFrame: contentFrame];
        [documentSplitView setAutoresizingMask:
                            NSViewWidthSizable | NSViewHeightSizable];
        [[window contentView] addSubview: documentSplitView];
        
        [documentSplitView newContentSubviewAtIndex: 0
                           alongAxis: UncommittedSplitAxis];
        
        [documentSplitView adjustSubviews];
    }
    return self;
}


- (void) sizeToDefault {
    CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
    CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
    
    CGFloat dividerHeight
        = [DocumentSplitView minimumDividerThicknessForAxis:
                              VerticalSplitAxis];
    CGFloat dividerWidth
        = [DocumentSplitView minimumDividerThicknessForAxis:
                              HorizontalSplitAxis];
    CGFloat newWidth
        = dividerWidth
          + [DocumentContentView leftMarginWidth]
          + emWidth * 80.0
          + [DocumentContentView rightMarginWidth];
    CGFloat newHeight = 50.0 * lineHeight + dividerHeight;
    
    [self adjustSize: NSMakeSize(newWidth, newHeight)];
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
    NSWindow *window = [self window];
    NSRect frame = [window frame];
    
    CGFloat widthDifference = newSize.width - frame.size.width;
    frame.origin.x -= widthDifference / 2.0;
    
    CGFloat heightDifference = newSize.height - frame.size.height;
    frame.origin.y -= heightDifference;
    
    frame.size = newSize;
    
    [window setFrame: frame display: YES];
}


- (BOOL) getCurrentFolderInodeID: (uuid_t *) result {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return NO;
    
    // TODO
    return NO;
}

@end
