#import "Window/Browser/Item.h"

@implementation WindowBrowserItem
@synthesize path;

- (id) initWithWindowBrowserObject: (WindowBrowser *) newWindowBrowserObject
                           inodeID: (uuid_t *) newInodeID;
{
    self = [super init];
    if(self) {
        copyUUID(&inodeID, newInodeID);
        windowBrowserObject = newWindowBrowserObject;
    }
    return self;
}


- (uuid_t *) inodeID {
    return &inodeID;
}

@end
