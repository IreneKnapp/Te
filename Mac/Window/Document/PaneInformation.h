#import <Cocoa/Cocoa.h>
#import "Utilities.h"


@interface WindowDocumentPaneInformation : NSObject
{
    uuid_t uuid;
    NSWindow *window;
    NSRect frame;
    NSScroller *verticalScroller;
    NSScroller *horizontalScroller;
    NSTimer *scrollerHidingTimer;
}
@property (assign) NSWindow *window;
@property (assign) NSRect frame;
@property (assign) NSScroller *verticalScroller;
@property (assign) NSScroller *horizontalScroller;
@property (assign) NSTimer *scrollerHidingTimer;

- (id)  initWithUUID: (uuid_t *) newUUID
              window: (NSWindow *) newWindow
               frame: (NSRect) newFrame
    verticalScroller: (NSScroller *) newVerticalScroller
  horizontalScroller: (NSScroller *) newHorizontalScroller
 scrollerHidingTimer: (NSTimer *) newScrollerHidingTimer;
- (uuid_t *) uuid;
- (void) setUUID: (uuid_t *) newUUID;
@end
