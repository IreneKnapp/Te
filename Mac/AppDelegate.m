#import "AppDelegate.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"

@implementation AppDelegate

int argc;
char **argv;

- (void) applicationWillFinishLaunching: (NSNotification *) notification {
    argc = 1;
    argv = malloc(sizeof(char *));
    argv[0] = "Te";
    hs_init(&argc, &argv);
    
    applicationState = application_init((HsFunPtr) exception,
                                        (HsFunPtr) noteRecentProjectsChanged);
    
    char *versionLabelCString = version_string();
    NSString *versionLabelString
        = [NSString stringWithUTF8String: versionLabelCString];
    string_free(versionLabelCString);
    [metaProjectVersionLabel setStringValue: versionLabelString];
    
    [metaProjectRecentList setIntercellSpacing: NSMakeSize(0.0, 0.0)];
    [metaProjectRecentList setTarget: self];
    [metaProjectRecentList setDoubleAction: @selector(openRecentProject:)];
}


- (void) applicationWillTerminate: (NSNotification *) notification {
    application_exit(applicationState);
    hs_exit();
}


- (void) applicationDidFinishLaunching: (NSNotification *) notification {
    [metaProjectWindow makeKeyAndOrderFront: self];
}


- (NSInteger) numberOfRowsInTableView: (NSTableView *) tableView {
    if([tableView isEqual: metaProjectRecentList]) {
        uint64_t count = application_recent_project_count(applicationState);
        if(count == 0) {
            return 1;
        } else {
            return count;
        }
    } else {
        return 0;
    }
}


- (id)            tableView: (NSTableView *) tableView
  objectValueForTableColumn: (NSTableColumn *) tableColumn
                        row: (NSInteger) rowIndex
{
    if([tableView isEqual: metaProjectRecentList]) {
        if(application_recent_project_count(applicationState) == 0) {
            if([tableColumn isEqual: metaProjectRecentListIconColumn]) {
                return [NSImage imageNamed: @"FirstRunAttentionArrow"];
            } else if([tableColumn isEqual: metaProjectRecentListTextColumn]) {
                NSString *string = @"Recent projects\nwill appear here.";
                
                NSMutableDictionary *attributes
                    = [NSMutableDictionary dictionaryWithCapacity: 1];
                
                NSColor *foregroundColor = [NSColor disabledControlTextColor];
                [attributes setObject: foregroundColor
                            forKey: NSForegroundColorAttributeName];
                
                NSAttributedString *attributedString
                    = [[NSAttributedString alloc] initWithString: string
                                                  attributes: attributes];
                return attributedString;
            } else {
                return nil;
            }
        } else {
            if([tableColumn isEqual: metaProjectRecentListIconColumn]) {
                return [NSImage imageNamed: @"Project"];
            } else if([tableColumn isEqual: metaProjectRecentListTextColumn]) {
                return @"Foo";
            } else {
                return nil;
            }
        }
    } else {
        return nil;
    }
}


- (BOOL) tableView: (NSTableView *) tableView
   shouldSelectRow: (NSInteger) rowIndex
{
    if([tableView isEqual: metaProjectRecentList]) {
        if(YES) {
            return NO;
        } else {
            return YES;
        }
    } else {
        return YES;
    }
}


- (IBAction) newProject: (id) sender {
  application_new_project(applicationState);
}


- (IBAction) openProject: (id) sender {
  application_open_project(applicationState, "/nonexistent/fooze.te");
}


- (IBAction) openRecentProject: (id) sender {
    if([sender isEqual: metaProjectRecentList]) {
        NSInteger row = [metaProjectRecentList clickedRow];
        if(-1 != row) {
            uint64_t index = (uint64_t) row;
            application_open_recent_project(applicationState, index);
        }
    }
}


- (void) exception: (NSString *) string {
    NSLog(@"Exception: %@", string);
}


void exception(char *cString) {
    NSString *string = [NSString stringWithUTF8String: cString];
    [(AppDelegate *) [NSApp delegate] exception: string];
}


- (void) noteRecentProjectsChanged {
    [metaProjectRecentList reloadData];
}


void noteRecentProjectsChanged() {
    [(AppDelegate *) [NSApp delegate] noteRecentProjectsChanged];
}

@end
