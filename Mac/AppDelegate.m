#import "AppDelegate.h"
#import <HsFFI.h>

@implementation AppDelegate

int argc;
char **argv;

- (void) applicationWillFinishLaunching: (NSNotification *) notification {
    argc = 1;
    argv = malloc(sizeof(char *));
    argv[0] = "Te";
    hs_init(&argc, &argv);
}


- (void) applicationWillTerminate: (NSNotification *) notification {
    hs_exit();
}


- (void) applicationDidFinishLaunching: (NSNotification *) notification {
    [metaProjectWindow makeKeyAndOrderFront: self];
}

@end
