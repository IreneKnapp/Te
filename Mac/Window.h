#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@class FileNameCellFieldEditor;
@interface Window : NSWindowController <NSWindowDelegate>
{
    BOOL alreadyClosing;
    uuid_t windowID;
}
@property (assign) BOOL alreadyClosing;
@property (retain) FileNameCellFieldEditor *fileNameCellFieldEditor;

- (void) initHelper: (uuid_t *) newWindowID;
- (id) initWithWindowID: (uuid_t *) newWindowID
                nibName: (NSString *) nibName;
- (id) initWithWindowID: (uuid_t *) newWindowID
                 window: (NSWindow *) newWindow;
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
