#import "Project.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "AppDelegate.h"


@implementation Project

- (id) init {
    self = [super init];
    if(self) {
        setUUIDNull(&projectID);
    }
    return self;
}


- (id) initWithType: (NSString *) typeName error: (NSError **) outError {
    self = [self init];
    if(self) {
        NSLog(@"Attempting to make a Project with no file.");
    }
    return self;
}


- (id) initWithContentsOfURL: (NSURL *) absoluteURL
                      ofType: (NSString *) typeName
                       error: (NSError **) outError
{
    self = [self init];
    if(self) {
        NSLog(@"Attempting to make a Project from %@.", absoluteURL);
    }
    return self;
}


- (id) initWithProjectID: (uuid_t *) newProjectID {
    self = [self init];
    if(self) {
        copyUUID(&projectID, newProjectID);
    } else {
        NSLog(@"Huh?");
        void *applicationState
            = [(AppDelegate *) [NSApp delegate] applicationState];
        teProjectClose(applicationState, newProjectID);
    }
    return self;
}

@end
