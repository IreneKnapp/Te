#import "BrowserWindow.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"


@implementation BrowserWindow

- (id) initWithBrowserWindowID: (uuid_t *) newBrowserWindowID {
    self = [super initWithWindowNibName: @"BrowserWindow"];
    if(self) {
        copyUUID(&browserWindowID, newBrowserWindowID);
        [[self window] makeKeyAndOrderFront: self];
    } else {
        void *applicationState
            = [(AppDelegate *) [NSApp delegate] applicationState];
        teFrontEndInternalFailure(applicationState, __FILE__, __LINE__);
    }
    return self;
}

@end
