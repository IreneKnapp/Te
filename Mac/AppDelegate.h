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
    NSPointerFunctions *keyFunctions;
    NSPointerFunctions *valueFunctions;
    NSMapTable *browserWindows;
    
    int hsArgc;
    char **hsArgv;
}
@property (assign) void *applicationState;

- (NSMapTable *) newMapTable;
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
- (IBAction) newFolder: (id) sender;
- (IBAction) newHaskellModule: (id) sender;
- (IBAction) deleteFolderOrFile: (id) sender;
- (void) exceptionWithMessage: (NSString *) messageString
                      details: (NSString *) detailsString;
void exception(char *messageCString, char *detailsCString);
- (uint64_t) confirm: (void *) confirmationDialog
   completionHandler: (void (*)(uint64_t result)) completionHandler;
void confirm(void *confirmationDialog,
             void (*completionHandler)(uint64_t result));
- (void) noteRecentProjectsChanged;
void noteRecentProjectsChanged();
- (void) noteNewBrowserWindow: (uuid_t *) browserWindowID;
void noteNewBrowserWindow(uuid_t *browserWindowID);
- (void) noteDeletedBrowserWindow: (uuid_t *) browserWindowID;
void noteDeletedBrowserWindow(uuid_t *browserWindowID);
- (void) noteBrowserItemsChangedInBrowserWindow: (uuid_t *) browserWindowID;
void noteBrowserItemsChanged(uuid_t *browserWindowID);
- (void) editBrowserItemNameInBrowserWindow: (uuid_t *) browserWindowID
                                      inode: (uuid_t *) inodeID;
void editBrowserItemName(uuid_t *browserWindowID, uuid_t *inodeID);
@end
