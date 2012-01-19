#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@interface WindowDocumentView : NSView
{
}

- (id) initWithFrame: (NSRect) frame;
- (void) drawRect: (NSRect) dirtyRect;
- (BOOL) isFlipped;
- (void) resetCursorRects;
@end
