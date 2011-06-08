#import "DocumentWindow.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"
#import "Utilities.h"


@implementation DocumentWindow

- (id) initWithWindowID: (uuid_t *) newWindowID {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return nil;
    
    self = [super initWithWindowID: newWindowID nibName: @"DocumentWindow"];
    if(self) {
    }
    return self;
}


- (BOOL) getCurrentFolderInodeID: (uuid_t *) result {
    void *applicationState = getApplicationState();
    if(!applicationState)
        return NO;
    
    // TODO
    return NO;
}

@end
