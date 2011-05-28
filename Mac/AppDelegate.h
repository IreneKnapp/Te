#import <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject
{
    IBOutlet NSWindow *metaProjectWindow;
}

- (void) applicationWillFinishLaunching: (NSNotification *) notification;
- (void) applicationWillTerminate: (NSNotification *) notification;
- (void) applicationDidFinishLaunching: (NSNotification *) notification;
@end
