#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@class BrowserItem;
@interface Window : NSWindowController <NSWindowDelegate>
{
    BOOL alreadyClosing;
    uuid_t windowID;
}
@property (assign) BOOL alreadyClosing;

- (id) initWithWindowID: (uuid_t *) newWindowID
                nibName: (NSString *) nibName;
- (uuid_t *) windowID;
- (BOOL) windowShouldClose: (id) sender;
- (void) windowWillClose: (NSNotification *) notification;
- (void) forceClose;
- (void) runSheetModalAlert: (NSAlert *) alert
          completionHandler: (void (*)(uint64_t result)) completionHandler;
- (void) sheetModalAlertFinished: (NSAlert *) alert
                      returnCode: (NSInteger) returnCode
                     contextInfo: (void *) contextInfo;
- (BOOL)               window: (NSWindow *) window
  shouldDragDocumentWithEvent: (NSEvent *) event
                         from: (NSPoint) dragImageLocation
               withPasteboard: (NSPasteboard *) pasteboard;
- (BOOL)               window: (NSWindow *) window
  shouldPopUpDocumentPathMenu: (NSMenu *) menu;
@end
