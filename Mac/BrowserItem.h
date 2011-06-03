#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@class BrowserWindow;
@interface BrowserItem : NSObject {
    uuid_t inodeID;
    BrowserWindow *browserWindowObject;
}
@property (assign) NSString *path;

- (id) initWithBrowserWindowObject: (BrowserWindow *) newBrowserWindowObject
                           inodeID: (uuid_t *) newInodeID;
- (uuid_t *) inodeID;
@end
