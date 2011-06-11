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
        CGFloat emWidth = [(AppDelegate *) [NSApp delegate] emWidth];
        CGFloat lineHeight = [(AppDelegate *) [NSApp delegate] lineHeight];
        
        NSRect frame = [window frame];
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
        NSUInteger lineCount
            = floor((frame.size.height - dividerHeight) / lineHeight);
        CGFloat newHeight = lineCount * lineHeight + dividerHeight;
        
        CGFloat widthDifference = newWidth - frame.size.width;
        frame.origin.x -= widthDifference / 2.0;
        frame.size.width = newWidth;
        
        CGFloat heightDifference = newHeight - frame.size.height;
        frame.origin.y -= heightDifference;
        frame.size.height = newHeight;
        
        [window setFrame: frame display: YES];
        
        NSSize contentMinSize = [window contentMinSize];
        contentMinSize.width = newWidth;
        [window setContentMinSize: contentMinSize];
        NSSize contentMaxSize = [window contentMaxSize];
        contentMaxSize.width = newWidth;
        [window setMaxSize: contentMaxSize];
        [window setContentResizeIncrements: NSMakeSize(1.0, lineHeight)];
        
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


- (BOOL) getCurrentFolderInodeID: (uuid_t *) result {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return NO;
    
    // TODO
    return NO;
}

@end
