#import "AppDelegate.h"
#import <HsFFI.h>
#import "Te/ForeignInterface_stub.h"
#import "Project.h"

@implementation AppDelegate
@synthesize applicationState;

- (void) applicationWillFinishLaunching: (NSNotification *) notification {
    hsArgc = 1;
    hsArgv = malloc(sizeof(char *));
    hsArgv[0] = "Te";
    hs_init(&hsArgc, &hsArgv);
    
    applicationState = teApplicationInit((HsFunPtr) exception,
                                         (HsFunPtr) noteRecentProjectsChanged,
                                         (HsFunPtr) noteNewProject,
                                         (HsFunPtr) noteDeletedProject,
                                         (HsFunPtr) noteNewBrowserWindow,
                                         (HsFunPtr) noteDeletedBrowserWindow);
    
    NSPointerFunctions *keyFunctions
        = [NSPointerFunctions
            pointerFunctionsWithOptions: NSMapTableCopyIn
                                         | NSMapTableStrongMemory];
    [keyFunctions setHashFunction: uuidHashPointerFunction];
    [keyFunctions setIsEqualFunction: uuidIsEqualPointerFunction];
    [keyFunctions setSizeFunction: uuidSizePointerFunction];
    [keyFunctions setDescriptionFunction: uuidDescriptionPointerFunction];
    [keyFunctions setAcquireFunction: opaqueAcquirePointerFunction];
    [keyFunctions setRelinquishFunction: opaqueRelinquishPointerFunction];
    
    NSPointerFunctions *valueFunctions
        = [NSPointerFunctions
            pointerFunctionsWithOptions: NSMapTableStrongMemory
                                         | NSMapTableObjectPointerPersonality];
    
    projects = [[NSMapTable alloc]
                initWithKeyPointerFunctions: keyFunctions
                valuePointerFunctions: valueFunctions
                capacity: 1024];
    
    char *versionLabelCString = teVersionString();
    NSString *versionLabelString
        = [NSString stringWithUTF8String: versionLabelCString];
    teStringFree(versionLabelCString);
    [metaProjectVersionLabel setStringValue: versionLabelString];
    
    [metaProjectRecentList setIntercellSpacing: NSMakeSize(0.0, 0.0)];
    [metaProjectRecentList setTarget: self];
    [metaProjectRecentList setDoubleAction: @selector(openRecentProject:)];
    
    openPanel = [NSOpenPanel openPanel];
    NSMutableArray *fileTypes = [NSMutableArray arrayWithCapacity: 1];
    [fileTypes addObject: @"com.dankna.te.project"];
    [openPanel setAllowedFileTypes: fileTypes];
    [openPanel setCanChooseFiles: YES];
    [openPanel setCanChooseDirectories: NO];
    [openPanel setAllowsMultipleSelection: YES];
    [openPanel setResolvesAliases: YES];
}


- (void) applicationWillTerminate: (NSNotification *) notification {
    teApplicationExit(applicationState);
    hs_exit();
}


- (void) applicationDidFinishLaunching: (NSNotification *) notification {
    [metaProjectWindow makeKeyAndOrderFront: self];
}


- (BOOL) applicationShouldOpenUntitledFile: (NSApplication *) sender {
    return NO;
}


- (NSInteger) numberOfRowsInTableView: (NSTableView *) tableView {
    if([tableView isEqual: metaProjectRecentList]) {
        uint64_t count = teApplicationRecentProjectCount(applicationState);
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
        if(teApplicationRecentProjectCount(applicationState) == 0) {
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
    teApplicationNewProject(applicationState);
}


- (IBAction) openProject: (id) sender {
    [openPanel beginWithCompletionHandler:
        ^(NSInteger result) {
            if(result != NSFileHandlingPanelOKButton) 
                return;
            
            for(NSURL *url in [openPanel URLs]) {
                if(![url isFileURL])
                    continue;
                
                NSString *path = [url path];
                char *pathCString = (char *) [path UTF8String];
                teApplicationOpenProject(applicationState, pathCString);
            }
        }];
}


- (IBAction) openRecentProject: (id) sender {
    if([sender isEqual: metaProjectRecentList]) {
        NSInteger row = [metaProjectRecentList clickedRow];
        if(-1 != row) {
            uint64_t index = (uint64_t) row;
            teApplicationOpenRecentProject(applicationState, index);
        }
    }
}


- (void) exceptionWithMessage: (NSString *) messageString
                      details: (NSString *) detailsString
{
    NSAlert *alert = [[NSAlert alloc] init];
    [alert setMessageText: messageString];
    [alert setInformativeText: detailsString];
    [alert addButtonWithTitle: NSLocalizedString(@"Okay", @"Okay")];
    [alert runModal];
}


void exception(char *messageCString, char *detailsCString) {
    NSString *messageString = [NSString stringWithUTF8String: messageCString];
    NSString *detailsString = [NSString stringWithUTF8String: detailsCString];
    [(AppDelegate *) [NSApp delegate] exceptionWithMessage: messageString
                                      details: detailsString];
}


- (void) noteRecentProjectsChanged {
    [metaProjectRecentList reloadData];
}


void noteRecentProjectsChanged() {
    [(AppDelegate *) [NSApp delegate] noteRecentProjectsChanged];
}


- (void) noteNewProject: (uuid_t *) projectID {
    Project *projectObject = [[Project alloc] initWithProjectID: projectID];
    if(projectObject)
        [projects setObject: projectObject forKey: (void *) projectID];
}


void noteNewProject(uuid_t *projectID) {
    [(AppDelegate *) [NSApp delegate] noteNewProject: projectID];
}


- (void) noteDeletedProject: (uuid_t *) projectID {
}


void noteDeletedProject(uuid_t *projectID) {
    [(AppDelegate *) [NSApp delegate] noteDeletedProject: projectID];
}


- (void) noteNewBrowserWindow: (uuid_t *) browserWindowID
                   forProject: (uuid_t *) projectID
{
    NSLog(@"Mm-hm.  (I'll get right on that.)");
}


void noteNewBrowserWindow(uuid_t *projectID, uuid_t *browserWindowID) {
    [(AppDelegate *) [NSApp delegate] noteNewBrowserWindow: browserWindowID
                                      forProject: projectID];
}


- (void) noteDeletedBrowserWindow: (uuid_t *) browserWindowID {
}


void noteDeletedBrowserWindow(uuid_t *browserWindowID) {
    [(AppDelegate *) [NSApp delegate] noteDeletedBrowserWindow:
                                       browserWindowID];
}

@end
