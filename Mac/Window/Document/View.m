#import "Window/Document/View.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "Window/Document/PaneManager.h"


@implementation WindowDocumentView

- (id) initWithFrame: (NSRect) frame {
    self = [super initWithFrame: frame];
    if(self) {
        [self setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];
    }
    return self;
}


- (void) drawRect: (NSRect) dirtyRect {
    NSWindow *window = [self window];
    
    WindowDocumentPaneManager *paneManager
        = [WindowDocumentPaneManager sharedManager];
    [paneManager drawRect: dirtyRect ofWindow: window];
}


- (BOOL) isFlipped {
    return YES;
}

@end
