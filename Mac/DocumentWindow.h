#import <Cocoa/Cocoa.h>
#import "Window.h"
#import "HasCurrentFolder.h"
#import "Utilities.h"

@interface DocumentWindow : Window
<NSOutlineViewDataSource, HasCurrentFolder>
{
}

- (id) initWithWindowID: (uuid_t *) newWindowID;
- (BOOL) getCurrentFolderInodeID: (uuid_t *) result;
@end
