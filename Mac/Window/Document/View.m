#import "Window/Document/View.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "Window/Document.h"
#import "Window/Document/HorizontalDividerManager.h"
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
    
    WindowDocumentHorizontalDividerManager *horizontalDividerManager
        = [WindowDocumentHorizontalDividerManager sharedManager];
    [horizontalDividerManager drawRect: dirtyRect ofWindow: window];
}


- (BOOL) isFlipped {
    return YES;
}


- (void) resetCursorRects {
    void *applicationState = getApplicationState();
    if(!applicationState) return;
    
    NSWindow *window = [self window];
    if(!window) return;
    
    WindowDocument *windowDocumentObject = [window windowController];
    if(!windowDocumentObject) return;
    
    uuid_t *documentWindowID = [windowDocumentObject windowID];
    
    void *cursorRectangles
        = teDocumentWindowCursorRectangles(applicationState, documentWindowID);
    if(!cursorRectangles) return;
    
    uint64_t count = teCursorRectangleListCount(cursorRectangles);
    
    for(uint64_t i = 0; i < count; i++) {
        uint64_t cursorType;
        int64_t left, top, width, height;
        teCursorRectangleListItem(cursorRectangles, i, &cursorType,
                                  &left, &top, &width, &height);
        
        NSCursor *cursor = [NSCursor arrowCursor];
        switch(cursorType) {
        case ArrowCursorType:
            cursor = [NSCursor arrowCursor];
            break;
        case ResizeUpCursorType:
            cursor = [NSCursor resizeUpCursor];
            break;
        case ResizeDownCursorType:
            cursor = [NSCursor resizeDownCursor];
            break;
        case ResizeLeftCursorType:
            cursor = [NSCursor resizeLeftCursor];
            break;
        case ResizeRightCursorType:
            cursor = [NSCursor resizeRightCursor];
            break;
        case ResizeUpDownCursorType:
            cursor = [NSCursor resizeUpDownCursor];
            break;
        case ResizeLeftRightCursorType:
            cursor = [NSCursor resizeLeftRightCursor];
            break;
        }
        
        NSRect frame;
        frame.origin.x = left;
        frame.origin.y = top;
        frame.size.width = width;
        frame.size.height = height;
        
        [self addCursorRect: frame cursor: cursor];
    }
    
    teCursorRectangleListFree(cursorRectangles);
}

@end
