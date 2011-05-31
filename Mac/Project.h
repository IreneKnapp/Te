#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@interface Project : NSDocument
{
    uuid_t projectID;
}

- (id) init;
- (id) initWithType: (NSString *) typeName error: (NSError **) outError;
- (id) initWithContentsOfURL: (NSURL *) absoluteURL
                      ofType: (NSString *) typeName
                       error: (NSError **) outError;
- (id) initWithProjectID: (uuid_t *) newProjectID;
@end
