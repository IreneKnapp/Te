#import <Foundation/Foundation.h>
#import "Utilities.h"

@protocol HasCurrentFolder
- (BOOL) getCurrentFolderInodeID: (uuid_t *) result;
@end
