#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@class WindowBrowser;
@interface WindowBrowserItem : NSObject {
    uuid_t inodeID;
    WindowBrowser *windowBrowserObject;
}
@property (assign) NSString *path;

- (id) initWithWindowBrowserObject: (WindowBrowser *) newBrowserWindowObject
                           inodeID: (uuid_t *) newInodeID;
- (uuid_t *) inodeID;
@end
