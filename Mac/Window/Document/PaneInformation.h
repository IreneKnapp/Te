#import <Cocoa/Cocoa.h>
#import "Utilities.h"


@interface WindowDocumentPaneInformation : NSObject
{
    uuid_t uuid;
}
@property (retain) NSWindow *window;
@property (assign) NSRect frame;
@property (retain) NSScroller *verticalScroller;
@property (retain) NSScroller *horizontalScroller;
@property (retain) NSTimer *scrollerHidingTimer;

- (id)  initWithUUID: (uuid_t *) newUUID
              window: (NSWindow *) newWindow
               frame: (NSRect) newFrame
    verticalScroller: (NSScroller *) newVerticalScroller
  horizontalScroller: (NSScroller *) newHorizontalScroller
 scrollerHidingTimer: (NSTimer *) newScrollerHidingTimer;
- (uuid_t *) uuid;
- (void) setUUID: (uuid_t *) newUUID;
@end
