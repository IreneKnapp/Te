#import <Cocoa/Cocoa.h>
#import "Utilities.h"

@interface AppDelegate : NSDocumentController
<NSApplicationDelegate, NSTableViewDataSource, NSTableViewDelegate>
{
    IBOutlet NSWindow *metaProjectWindow;
    IBOutlet NSTextField *metaProjectVersionLabel;
    IBOutlet NSTableView *metaProjectRecentList;
    IBOutlet NSTableColumn *metaProjectRecentListIconColumn;
    IBOutlet NSTableColumn *metaProjectRecentListTextColumn;
    
    NSOpenPanel *openPanel;
    
    void *applicationState;
    NSMapTable *projects;
    
    int hsArgc;
    char **hsArgv;
}
@property (assign) void *applicationState;

- (void) applicationWillFinishLaunching: (NSNotification *) notification;
- (void) applicationWillTerminate: (NSNotification *) notification;
- (void) applicationDidFinishLaunching: (NSNotification *) notification;
- (BOOL) applicationShouldOpenUntitledFile: (NSApplication *) sender;
- (NSInteger) numberOfRowsInTableView: (NSTableView *) tableView;
- (id)            tableView: (NSTableView *) tableView
  objectValueForTableColumn: (NSTableColumn *) tableColumn
                        row: (NSInteger) rowIndex;
- (BOOL) tableView: (NSTableView *) tableView
   shouldSelectRow: (NSInteger) rowIndex;
- (IBAction) newProject: (id) sender;
- (IBAction) openProject: (id) sender;
- (IBAction) openRecentProject: (id) sender;
- (void) exceptionWithMessage: (NSString *) messageString
                      details: (NSString *) detailsString;
void exception(char *messageCString, char *detailsCString);
- (void) noteRecentProjectsChanged;
void noteRecentProjectsChanged();
- (void) noteNewProject: (uuid_t *) projectID;
void noteNewProject(uuid_t *projectID);
- (void) noteDeletedProject: (uuid_t *) projectID;
void noteDeletedProject(uuid_t *projectID);
- (void) noteNewBrowserWindow: (uuid_t *) browserWindowID
                   forProject: (uuid_t *) projectID;
void noteNewBrowserWindow(uuid_t *projectID, uuid_t *browserWindowID);
- (void) noteDeletedBrowserWindow: (uuid_t *) browserWindowID;
void noteDeletedBrowserWindow(uuid_t *browserWindowID);
@end
