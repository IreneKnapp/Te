#import "BrowserItem.h"

@implementation BrowserItem
@synthesize path;

- (id) initWithBrowserWindowObject: (BrowserWindow *) newBrowserWindowObject
                           inodeID: (uuid_t *) newInodeID;
{
    self = [super init];
    if(self) {
        copyUUID(&inodeID, newInodeID);
        browserWindowObject = newBrowserWindowObject;
    }
    return self;
}


- (uuid_t *) inodeID {
    return &inodeID;
}

@end
