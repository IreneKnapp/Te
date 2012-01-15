#import "Window/Document/PaneInformation.h"


@implementation WindowDocumentPaneInformation
@synthesize window;
@synthesize frame;
@synthesize verticalScroller;
@synthesize horizontalScroller;
@synthesize scrollerHidingTimer;

- (id)  initWithUUID: (uuid_t *) newUUID
              window: (NSWindow *) newWindow
               frame: (NSRect) newFrame
    verticalScroller: (NSScroller *) newVerticalScroller
  horizontalScroller: (NSScroller *) newHorizontalScroller
 scrollerHidingTimer: (NSTimer *) newScrollerHidingTimer
{
    self = [super init];
    if(self) {
        copyUUID(&uuid, newUUID);
        window = newWindow;
        frame = newFrame;
        verticalScroller = newVerticalScroller;
        horizontalScroller = newHorizontalScroller;
        scrollerHidingTimer = newScrollerHidingTimer;
    }
    return self;
}


- (uuid_t *) uuid {
    return &uuid;
}


- (void) setUUID: (uuid_t *) newUUID {
    copyUUID(&uuid, newUUID);
}

@end