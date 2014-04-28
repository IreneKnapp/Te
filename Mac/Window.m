#import "Window.h"
#import <HsFFI.h>
#import "Te/LowLevel/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "Utilities.h"


@implementation Window
@synthesize alreadyClosing;
@synthesize fileNameCellFieldEditor;


- (void) initHelper: (uuid_t *) newWindowID {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return;
    
    copyUUID(&windowID, newWindowID);
    
    alreadyClosing = NO;
    fileNameCellFieldEditor = nil;
    
    char *titleCString = teWindowTitle(applicationState, &windowID);
    NSString *title = @"";
    if(titleCString) {
        title = [NSString stringWithUTF8String: titleCString];
        teTextFree(titleCString);
    }
    
    char *titleIconCString
        = teWindowTitleIcon(applicationState, &windowID);
    NSString *titleIcon = @"";
    if(titleIconCString) {
        titleIcon = [NSString stringWithUTF8String: titleIconCString];
        teTextFree(titleIconCString);
    }
    
    NSWindow *window = [self window];
    [window makeKeyAndOrderFront: self];
    [window setTitle: title];
    [window setRepresentedURL: [NSURL URLWithString: @""]];
    [[window standardWindowButton: NSWindowDocumentIconButton]
     setImage: [NSImage imageNamed: titleIcon]];
}


- (id) initWithWindowID: (uuid_t *) newWindowID
                nibName: (NSString *) nibName
{
    void *applicationState = getApplicationState();
    if(!applicationState)
        return nil;
    
    self = [super initWithWindowNibName: nibName];
    if(self) {
        [self initHelper: newWindowID];
    } else {
        teFrontEndInternalFailure(applicationState, __FILE__, __LINE__);
    }
    return self;
}


- (id) initWithWindowID: (uuid_t *) newWindowID
                 window: (NSWindow *) newWindow
{
    void *applicationState = getApplicationState();
    if(!applicationState)
        return nil;
    
    self = [super initWithWindow: newWindow];
    if(self) {
        [self initHelper: newWindowID];
    } else {
        teFrontEndInternalFailure(applicationState, __FILE__, __LINE__);
    }
    return self;
}


- (uuid_t *) windowID {
    return &windowID;
}


- (BOOL) windowShouldClose: (id) sender {
    return YES;
}


- (void) windowWillClose: (NSNotification *) notification {
    alreadyClosing = YES;
    
    void *applicationState = getApplicationState();
    if(!applicationState)
        return;
    
    teWindowClose(applicationState, &windowID);
}


- (void) forceClose {
    if(!alreadyClosing) {
        [[self window] close];
    }
}


- (void) runSheetModalAlert: (NSAlert *) alert
          completionHandler: (void (*)(uint64_t result)) completionHandler
{
    [alert beginSheetModalForWindow: [self window]
           modalDelegate: self
           didEndSelector:
            @selector(sheetModalAlertFinished:returnCode:contextInfo:)
           contextInfo: completionHandler];
}


- (void) sheetModalAlertFinished: (NSAlert *) alert
                      returnCode: (NSInteger) returnCode
                     contextInfo: (void *) contextInfo
{
    void (*completionHandler)(uint64_t result)
        = (void (*)(uint64_t result)) contextInfo;
    completionHandler(returnCode);
    teCompletionHandlerFree((HsFunPtr) completionHandler);
}


- (BOOL)               window: (NSWindow *) window
  shouldDragDocumentWithEvent: (NSEvent *) event
                         from: (NSPoint) dragImageLocation
               withPasteboard: (NSPasteboard *) pasteboard
{
    return NO;
}


- (BOOL)               window: (NSWindow *) window
  shouldPopUpDocumentPathMenu: (NSMenu *) menu
{
    return NO;
}

@end
